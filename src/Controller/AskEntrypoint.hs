module Controller.AskEntrypoint where

import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Maybe
import qualified Data.Aeson                    as Aeson
import           Control.Concurrent.Async
import           Control.Exception
import           Text.Printf

import qualified Helper.Telegram.Types         as Telegram
import qualified Helper.Telegram.SendMessage   as Telegram (markdownReplyMessage, replyMessage, SendMessage)
import           UseCase.Ask
import           Dataproviders.AskDataprovider
import qualified Entity.Question               as Q
import qualified Entity.Message                as M
import qualified Entity.Answer                 as A
import qualified Controller.SendMessage        as Send
import qualified Interface.ToMarkdown          as I

-- TODO: Change folder name from "Controller" to "Entrypoint"

instance I.ToMarkdown M.Message where
  markdown (M.ImageMessage img [] ) = printf "[image](%s)" img
  markdown (M.ImageMessage img cap) = printf "[%s](%s)" cap img
  markdown (M.TextMessage  text   ) = text

mainEntrypoint :: [Telegram.Update] -> IO ()
mainEntrypoint = mapConcurrently_ (handle handleEntrypoint . assignEntrypoint)
  where handleEntrypoint :: IOException -> IO ()
        handleEntrypoint = print

assignEntrypoint :: Telegram.Update -> IO ()
assignEntrypoint update = case command of
                            Just (cmd, after) -> do ansMsg <- callEntrypoint cmd after
                                                    sendResponseMessage msg ansMsg
                            Nothing           -> putStrLn "No command on update..."
  where command = getCommand msg
        msg     = Telegram.message update

getBotResponse :: Telegram.Update -> IO (Maybe Telegram.SendMessage)
getBotResponse update = fmap (getReplyMessage msg) <$> sequence (uncurry callEntrypoint <$> command)
  where command = getCommand msg
        msg     = Telegram.message update

callEntrypoint :: String -> String -> IO M.Message
callEntrypoint command content
  | command == "/wolfram" = askEntrypoint content
  | command == "/start" = return M.helloMessage
  | otherwise = fail "Nenhum comando com esse nome..."

answerToMessage :: A.Answer -> M.Message
answerToMessage ans
  = maybe (M.simpleMessage $ A.text ans) M.imageMessage (A.image ans)

askEntrypoint :: String -> IO M.Message
askEntrypoint text
  | filter (/= ' ') text == "" = return M.noContentMessage
  | otherwise = let question = Q.Question { Q.text = text } in
                do putStrLn $ show question
                   answer <- ask askDataproviderImplementation question
                   putStrLn $ show answer
                   return $ answerToMessage answer

sendResponseMessage :: Telegram.Message -> M.Message -> IO ()
sendResponseMessage msg = Send.sendMessage . getReplyMessage msg

getReplyMessage :: Telegram.Message -> M.Message -> Telegram.SendMessage
getReplyMessage msg ans
  = case ans of
      M.TextMessage  text    -> Telegram.replyMessage cid text mid
      M.ImageMessage img cap -> Telegram.markdownReplyMessage cid (I.markdown ans) mid
  where cid  = Telegram.chat_id chat
        mid  = Telegram.message_id msg
        chat = Telegram.chat msg

getCommand :: Telegram.Message -> Maybe (String, String)
getCommand msg = do entity <- head <$> Telegram.entities msg
                    let text    = Telegram.text msg
                    let len     = Telegram.entity_length entity
                    let off     = Telegram.entity_offset entity
                    let after   = drop (len + off) text
                    let command = take len $ drop off text
                    return (command, after)
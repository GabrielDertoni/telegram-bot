module Controller.AskEntrypoint where

import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Maybe
import qualified Data.Aeson                    as Aeson
import           Control.Concurrent.Async
import           Control.Exception
import           Text.Printf
import qualified Control.Concurrent.STM        as STM
import qualified Control.Concurrent.STM.TChan  as STM

import qualified Helper.Telegram               as Telegram (getCommand)
import qualified Helper.Telegram.Types         as Telegram
import qualified Helper.Telegram.SendMessage   as Telegram (markdownReplyMessage, replyMessage, SendMessage)
import           UseCase.Ask
import           Dataproviders.AskDataprovider
import qualified Entity.Question               as Q
import qualified Entity.Message                as M
import qualified Entity.Answer                 as A
import qualified Controller.SendMessage        as Send
import qualified Controller.InterpretBrainfuckEntrypoint as BF
import qualified Interface.ToMarkdown          as I

-- TODO: Change folder name from "Controller" to "Entrypoint"

instance I.ToMarkdown M.Message where
  markdown (M.ImageMessage img [] ) = printf "[image](%s)" img
  markdown (M.ImageMessage img cap) = printf "[%s](%s)" cap img
  markdown (M.TextMessage  text   ) = text
{-
mainEntrypoint :: [Telegram.Update] -> IO ()
mainEntrypoint = mapConcurrently_ (handle handleEntrypoint . assignEntrypoint)
  where handleEntrypoint :: IOException -> IO ()
        handleEntrypoint = print

assignEntrypoint :: Telegram.Update -> IO ()
assignEntrypoint update = case command of
                            Just (cmd, after) -> do ansMsg <- callEntrypoint cmd after update
                                                    sendResponseMessage msg ansMsg
                            Nothing           -> putStrLn "No command on update..."
  where command = getCommand msg
        msg     = Telegram.message update
-}
getBotResponse :: Telegram.Update -> STM.TChan Telegram.Update -> IO (Maybe Telegram.SendMessage)
getBotResponse update broadChan
  = case command of
      Nothing  -> return Nothing
      Just cmd -> do let (c, a) = cmd
                     ans <- callEntrypoint c a update broadChan
                     let rep = getReplyMessage msg <$> ans
                     return rep
  where command = Telegram.getCommand msg
        msg     = Telegram.message update

callEntrypoint :: String -> String -> Telegram.Update -> STM.TChan Telegram.Update -> IO (Maybe M.Message)
callEntrypoint command content update broadChan
  | command == "/wolfram" = Just <$> askEntrypoint content
  | command == "/brainfuck" = Just <$> BF.brainfuckEntrypoint content update broadChan
  | command == "/start" = return $ Just M.helloMessage
  | otherwise = return Nothing

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
sendResponseMessage msg ans = do Send.sendMessage $ getReplyMessage msg ans
                                 return ()

getReplyMessage :: Telegram.Message -> M.Message -> Telegram.SendMessage
getReplyMessage msg ans
  = case ans of
      M.TextMessage  text    -> Telegram.replyMessage cid text mid
      M.ImageMessage img cap -> Telegram.markdownReplyMessage cid (I.markdown ans) mid
  where cid  = Telegram.chat_id chat
        mid  = Telegram.message_id msg
        chat = Telegram.chat msg
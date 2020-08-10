module Controller.AskEntrypoint where

import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe
import qualified Data.Aeson as Aeson
import Control.Concurrent.Async
import Control.Exception

import qualified Helper.Telegram.Message   as Telegram
import qualified Helper.Telegram.Chat      as Telegram
import qualified Helper.Telegram.Update    as Telegram
import qualified Helper.Telegram.SendPhoto as Telegram (replyPhoto)
import UseCase.Ask
import Dataproviders.AskDataprovider
import qualified Entity.Question as Q
import qualified Entity.Message  as M
import qualified Entity.Answer   as A
import qualified Controller.SendMessage as Send

-- TODO: Change folder name from "Controller" to "Entrypoint"

main_entrypoint :: [Telegram.Update] -> IO ()
main_entrypoint updates = sequence_ $ map (\up -> catch (assign_entrypoint up) handleEntrypoint) updates
  where handleEntrypoint :: IOException -> IO ()
        handleEntrypoint = print

assign_entrypoint :: Telegram.Update -> IO ()
assign_entrypoint update = case command of
                            Just (cmd, after) -> do ansMsg <- call_entrypoint cmd after
                                                    sendResponseMessage msg ansMsg
                            -- Just (cmd, after) -> do putStrLn cmd
                            --                         putStrLn after
                            Nothing -> do putStrLn "No command on update..."
  where command = getCommand msg
        msg     = Telegram.message update

call_entrypoint :: String -> String -> IO M.Message
call_entrypoint command content
  | command == "/wolfram" = ask_entrypoint content
  | otherwise = fail "Nenhum comando com esse nome..."

answerToMessage :: A.Answer -> M.Message
answerToMessage ans
  = M.ImageMessage { M.image   = A.image ans
                   , M.caption = ""
                   }

ask_entrypoint :: String -> IO M.Message
ask_entrypoint text
  = let question = Q.Question { Q.text = text } in
    do putStrLn $ show question
       answer <- ask askDataproviderImplementation question
       putStrLn $ show answer
       return $ answerToMessage answer


sendResponseMessage :: Telegram.Message -> M.Message -> IO ()
sendResponseMessage msg (M.ImageMessage ansImg _)
  = let reply = Telegram.replyPhoto (Telegram.chat_id chat) ansImg (Telegram.message_id msg) in
    Send.sendPhoto reply
  where chat = Telegram.chat msg

getCommand :: Telegram.Message -> Maybe (String, String)
getCommand msg = do entity <- head <$> Telegram.entities msg
                    let text    = Telegram.text msg
                    let len     = Telegram.entity_length entity
                    let off     = Telegram.entity_offset entity
                    let after   = drop (len + off) text
                    let command = take len $ drop off text
                    return (command, after)
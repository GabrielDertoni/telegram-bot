module API.UpdateHandler where

import           Control.Exception
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson
import           Control.Monad.IO.Class
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as STM
import           Data.ByteString.Lazy.UTF8 (ByteString)

import           Interface.Entrypoint
import qualified Configuration.TelegramConfig as Telegram
import qualified Helper.Telegram.Types        as Telegram
import qualified Helper.Telegram              as Telegram
import qualified Helper.Telegram.SendMessage  as Method
import qualified Entity.Question              as Q
import qualified Entity.Message               as M
import qualified Entity.Answer                as A

import qualified Controller.SendMessage       as Send
import qualified Controller.AskEntrypoint     as Entrypoint
import qualified Controller.BrainfuckEntrypoint as Entrypoint


startCommand :: EntrypointM ()
startCommand = command "/start" $ do
  message <- getCallerMessage
  liftIO $ Send.sendSimpleReplyTo message "Olá você(s)"
  return Nothing

allEntrypoints :: EntrypointM ()
allEntrypoints = do
  startCommand
  Entrypoint.wolframCommand
  Entrypoint.brainfuckCommand

updateCallback :: STM.TChan Telegram.Update -> Scotty.ActionM ()
updateCallback broadChan = do
  req <- Scotty.body
  case decodeRequest req of
    Right update -> do liftIO $ STM.atomically $ STM.writeTChan broadChan update
                       liftIO $ sequenceEntrypoints allEntrypoints (update, broadChan)
                       Scotty.text "true"

    Left err     -> fail err

decodeRequest :: ByteString -> Either String Telegram.Update
decodeRequest = Aeson.eitherDecode

getUpdateHandlerPath :: IO String
getUpdateHandlerPath = do apiKey <- Telegram.getApiKey
                          return $ "/" <> apiKey <> "/update"

getUpdateHandler :: IO (Scotty.ScottyM ())
getUpdateHandler = do path <- getUpdateHandlerPath
                      broadChan <- STM.newBroadcastTChanIO
                      return $ Scotty.post (Scotty.capture path) $ updateCallback broadChan
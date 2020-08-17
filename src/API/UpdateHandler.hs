module API.UpdateHandler where

import           Control.Exception
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson
import           Control.Monad.IO.Class
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as STM
import           Data.ByteString.Lazy.UTF8 (ByteString)

import qualified Configuration.TelegramConfig as Telegram
import           Helper.Telegram.Types        as Telegram
import           Controller.AskEntrypoint

import qualified Controller.SendMessage as Send

updateCallback :: STM.TChan Telegram.Update -> Scotty.ActionM ()
updateCallback broadChan = do
  req <- Scotty.body
  case decodeRequest req of
    Right update -> do liftIO $ STM.atomically $ STM.writeTChan broadChan update
                       msg <- liftIO $ getBotResponse update broadChan
                       case msg of
                         Nothing -> Scotty.text "true"
                         Just m  -> do liftIO $ Send.sendMessage m
                                       Scotty.text "true"

    Left err     -> fail err

printCallback :: Scotty.ActionM ()
printCallback = do req <- Scotty.body
                   liftIO $ print req
                   Scotty.text "True"

decodeRequest :: ByteString -> Either String Update
decodeRequest = Aeson.eitherDecode

getUpdateHandlerPath :: IO String
getUpdateHandlerPath = do apiKey <- Telegram.getApiKey
                          return $ "/" <> apiKey <> "/update"

getUpdateHandler :: IO (Scotty.ScottyM ())
getUpdateHandler = do path <- getUpdateHandlerPath
                      broadChan <- STM.newBroadcastTChanIO
                      return $ Scotty.post (Scotty.capture path) $ updateCallback broadChan
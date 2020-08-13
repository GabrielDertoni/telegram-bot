module API.UpdateHandler where

import           Control.Exception
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.UTF8 (ByteString)

import qualified Configuration.TelegramConfig as Telegram
import           Helper.Telegram.Types
import           Controller.AskEntrypoint

updateCallback :: Scotty.ActionM ()
updateCallback = do
  req <- Scotty.body
  case decodeRequest req of
    Right update -> do msg <- liftIO $ getBotResponse update
                       sequence_ (Scotty.json <$> msg)
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
getUpdateHandler = do path <- liftIO $ getUpdateHandlerPath
                      return $ Scotty.post (Scotty.capture path) updateCallback
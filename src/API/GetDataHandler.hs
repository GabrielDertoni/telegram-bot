module API.GetDataHandler where

import qualified Web.Scotty as Scotty
import Control.Monad.IO.Class

import Dataproviders.BotInfo
import qualified Configuration.TelegramConfig as Telegram

getDataCallback :: Scotty.ActionM ()
getDataCallback  = do info <- liftIO $ getInfo
                      Scotty.json info

getGetDataHandler :: IO (Scotty.ScottyM ())
getGetDataHandler = do apiKey <- Telegram.getApiKey
                       return $ Scotty.get (Scotty.capture ("/" <> apiKey <> "/data")) getDataCallback
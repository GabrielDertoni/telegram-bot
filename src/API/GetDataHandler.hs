module API.GetDataHandler where

import qualified Web.Scotty as Scotty
import Control.Monad.IO.Class

import Dataproviders.BotInfo
import qualified Secret.TelegramAPI as Telegram

getDataCallback :: Scotty.ActionM ()
getDataCallback  = do info <- liftIO $ getInfo
                      Scotty.json info

getDataHandler :: Scotty.ScottyM ()
getDataHandler = Scotty.get (Scotty.capture ("/" <> Telegram.apiKey <> "/data")) getDataCallback
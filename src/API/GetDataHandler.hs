module API.GetDataHandler where

import           Control.Monad.IO.Class
import qualified Web.Scotty                   as Scotty

import qualified Interface.BotInfo            as I
import qualified Configuration.GlobalConfig   as Cfg
import qualified Configuration.TelegramConfig as Telegram

getDataCallback :: Scotty.ActionM ()
getDataCallback = do info <- liftIO $ I.getInfo $ Cfg.botInfoDataprovider Cfg.globalConfig
                     Scotty.json info

getGetDataHandler :: IO (Scotty.ScottyM ())
getGetDataHandler = do apiKey <- Telegram.getApiKey
                       return $ Scotty.get (Scotty.capture ("/" <> apiKey <> "/data")) getDataCallback
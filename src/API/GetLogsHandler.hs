module API.GetLogsHandler (getGetLogsHandler) where

import           Control.Monad.IO.Class
import qualified Web.Scotty                   as Scotty

import qualified Dataproviders.Logger         as Logger
import qualified Configuration.TelegramConfig as Telegram


getGetLogsHandler :: IO (Scotty.ScottyM ())
getGetLogsHandler = do path <- getGetLogsHandlerPath
                       return $ Scotty.get (Scotty.capture path) getLogsCallback

getLogsCallback :: Scotty.ActionM ()
getLogsCallback = do info <- liftIO $ Logger.getLogs
                     Scotty.json info

getGetLogsHandlerPath :: IO String
getGetLogsHandlerPath = do apiKey <- Telegram.getApiKey
                           return $ "/" <> apiKey <> "/logs"

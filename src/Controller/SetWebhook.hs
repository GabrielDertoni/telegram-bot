module Controller.SetWebhook where

import Network.HTTP.Conduit

import qualified Dataproviders.Logger as Logger
import Helper.Query
import Helper.Telegram.SetWebhook as Telegram

setWebhook :: Telegram.SetWebhook -> IO ()
setWebhook sw = do
  url <- getURL sw
  Logger.log "Setting webhook..."
  response <- simpleHttp url
  return ()
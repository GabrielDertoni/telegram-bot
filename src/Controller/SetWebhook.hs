module Controller.SetWebhook where

import Network.HTTP.Conduit

import Helper.Query
import Helper.Telegram.SetWebhook as Telegram

setWebhook :: Telegram.SetWebhook -> IO ()
setWebhook sw = do
  url <- getURL sw
  putStrLn "Setting webhook..."
  response <- simpleHttp url
  return ()
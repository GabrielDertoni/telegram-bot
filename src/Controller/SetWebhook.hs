module Controller.SetWebhook where

import Network.HTTP.Conduit

import Helper.Query
import Helper.Telegram.SetWebhook as Telegram

setWebhook :: Telegram.SetWebhook -> IO ()
setWebhook sw = do
  putStrLn "Setting webhook..."
  response <- simpleHttp $ getURL sw
  return ()
module Controller.SendMessage where

import qualified Network.URI.Encode as URI
import Network.HTTP.Conduit

import Entity.Message
import Helper.URL
import Helper.Query
import qualified Secret.TelegramAPI as Telegram
import qualified Helper.Telegram    as Telegram
import Helper.Telegram.SendPhoto    as Telegram

baseURL = "https://api.telegram.org/bot"

sendPhoto :: Telegram.SendPhoto -> IO ()
sendPhoto msg = do
  response <- simpleHttp $ getURL msg
  return ()
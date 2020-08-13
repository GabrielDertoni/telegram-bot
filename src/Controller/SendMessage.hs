module Controller.SendMessage where

import qualified Network.URI.Encode as URI
import Network.HTTP.Conduit
import Control.Exception

import           Entity.Message
import           Helper.Query
import qualified Helper.Telegram              as Telegram
import           Helper.Telegram.SendPhoto    as Telegram
import           Helper.Telegram.SendMessage  as Telegram

sendPhoto :: Telegram.SendPhoto -> IO ()
sendPhoto msg = do
  url <- getURL msg
  response <- simpleHttp $ url
  return ()

sendMessage :: Telegram.SendMessage -> IO ()
sendMessage msg = do
  url <- getURL msg
  putStrLn ("Send message URL: " <> url)
  response <- simpleHttp url
  return ()
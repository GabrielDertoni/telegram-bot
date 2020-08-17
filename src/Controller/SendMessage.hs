module Controller.SendMessage where

import qualified Network.URI.Encode as URI
import Network.HTTP.Conduit
import Control.Exception
import qualified Data.Aeson                   as Aeson

import           Entity.Message
import           Helper.Query
import qualified Helper.Telegram.Types        as Telegram
import qualified Helper.Telegram              as Telegram
import qualified Helper.Telegram.SendPhoto    as Telegram
import qualified Helper.Telegram.SendMessage  as Telegram

sendPhoto :: Telegram.SendPhoto -> IO ()
sendPhoto msg = do
  url <- getURL msg
  response <- simpleHttp $ url
  return ()

sendMessage :: Telegram.SendMessage -> IO Telegram.Message
sendMessage msg = do
  url <- getURL msg
  putStrLn ("Send message URL: " <> url)
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing  -> fail "No message in response"
    Just msg -> return $ Telegram.getSentMessage msg

telegramRequest :: (Query a, Aeson.FromJSON b) => a -> IO b
telegramRequest q = do
  url <- getURL q
  putStrLn ("Makin request to URL: " <> url)
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing  -> fail "Nothing in response." 
    Just res -> return $ Telegram.result res
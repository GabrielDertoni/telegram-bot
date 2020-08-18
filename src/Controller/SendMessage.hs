module Controller.SendMessage where

import           Network.HTTP.Conduit
import           Control.Exception
import           Text.Printf
import qualified Data.Aeson                      as Aeson
import qualified Network.URI.Encode              as URI

import qualified Dataproviders.Logger            as Logger
import           Entity.Message
import           Helper.Query
import qualified Helper.Telegram.Types           as Telegram
import qualified Helper.Telegram                 as Telegram
import qualified Helper.Telegram.SendPhoto       as Method
import qualified Helper.Telegram.SendMessage     as Method
import qualified Helper.Telegram.EditMessageText as Method
import qualified Interface.ToMarkdown            as I

instance I.ToMarkdown Message where
  markdown (ImageMessage [] img ) = printf "[image](%s)" img
  markdown (ImageMessage cap img) = printf "[%s](%s)" cap img
  markdown (TextMessage  text   ) = text

sendPhoto :: Method.SendPhoto -> IO ()
sendPhoto msg = do
  url <- getURL msg
  response <- simpleHttp $ url
  return ()

sendMessage :: Method.SendMessage -> IO Telegram.Message
sendMessage msg = do
  url <- getURL msg
  Logger.log $ "Send message URL: " <> url
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing   -> fail "No message in response"
    Just resp -> return $ Method.getSentMessage resp

editMessageText :: Method.EditMessageText -> IO Telegram.Message
editMessageText edit = do
  url <- getURL edit
  Logger.log ("Edit message URL: " <> url)
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing   -> fail "No message in response"
    Just resp -> return $ Method.getEditedMessage resp

getReplyMessage :: Telegram.Message -> Message -> Method.SendMessage
getReplyMessage msg ans
  = case ans of
      TextMessage  text    -> Method.replyMessage cid text mid
      ImageMessage img cap -> Method.markdownReplyMessage cid (I.markdown ans) mid
  where cid  = Telegram.chat_id chat
        mid  = Telegram.message_id msg
        chat = Telegram.chat msg

sendReplyTo :: Telegram.Message -> Message -> IO Telegram.Message
sendReplyTo msg = sendMessage . (getReplyMessage msg)

sendSimpleReplyTo :: Telegram.Message -> String -> IO Telegram.Message
sendSimpleReplyTo msg = (sendReplyTo msg) . simpleMessage

editSimpleMessage :: Telegram.Message -> String -> IO Telegram.Message
editSimpleMessage msg = editMessageText . (Method.editMessageText msg)

telegramRequest :: (Query a, Aeson.FromJSON b) => a -> IO b
telegramRequest q = do
  url <- getURL q
  Logger.log ("Makin request to URL: " <> url)
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing  -> fail "Nothing in response." 
    Just res -> return $ Telegram.result res
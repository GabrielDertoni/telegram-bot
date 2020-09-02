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
import qualified Interface.SendMessage           as I

instance I.ToMarkdown Message where
  markdown msg = case getMessageImage msg of
                  Just img -> case maybeGetMessageText msg of
                                Nothing  -> printf "[image](%s)" img
                                Just cap -> printf "[%s](%s)" cap img
                  Nothing  -> getMessageText msg

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
  case response `seq` Aeson.decode response of
    Nothing   -> fail "No message in response"
    Just resp -> return $ Method.getSentMessage resp

editMessageText :: Method.EditMessageText -> IO Telegram.Message
editMessageText edit = do
  url <- getURL edit
  Logger.log ("Edit message URL: " <> url)
  response <- simpleHttp url
  case response `seq` Aeson.decode response of
    Nothing   -> fail "No message in response"
    Just resp -> return $ Method.getEditedMessage resp

telegramRequest :: (Query a, Aeson.FromJSON b) => a -> IO b
telegramRequest q = do
  url <- getURL q
  Logger.log ("Making request to URL: " <> url)
  response <- simpleHttp url
  case Aeson.decode response of
    Nothing  -> fail "Nothing in response." 
    Just res -> return $ Telegram.result res

instance I.SendMessage IO where
  sendMessage msg@TextMessage{} = do
    sent <- case getMessageReplyId msg of
      Nothing     -> sendMessage $ Method.simpleMessage (getMessageChatId msg) (getMessageText msg)
      Just replid -> sendMessage $ Method.replyMessage (getMessageChatId msg) (getMessageText msg) replid
    return $ Telegram.getMessageIdentifier sent
  
  sendMessage msg@ImageMessage{} = undefined
  sendMessage msg@EditMessageText{} = do
    sent <- editMessageText $ Method.editMessageText (getMessageChatId msg) (edit_message_id msg) (getMessageText msg)
    return $ Telegram.getMessageIdentifier sent

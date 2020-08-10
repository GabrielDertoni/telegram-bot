module Helper.Telegram.SendMessage
  ( SendMessage(..)
  , simpleMessage
  , replyMessage
  , markdownReplyMessage
  ) where

import qualified Network.URI.Encode as URI

import Helper.Query
import Helper.URL
import qualified Helper.Telegram as Telegram
-- TODO: Implement support for reply_markup

data SendMessage
  = SendMessage { chat_id :: Int
                , text :: String
                , parse_mode :: Maybe String
                , disable_notification :: Maybe Bool
                , reply_to_message_id :: Maybe Int
                } deriving (Eq, Show)

instance Query SendMessage where
  getURL msg = Telegram.endpointURL "sendMessage" <> sendMessageQuery msg

sendMessageQuery :: SendMessage -> String
sendMessageQuery msg
    =  ( "chat_id"              <=> (show $ chat_id msg))
  <:>  ( "text"                 <=> (URI.encode $ text msg))
  <:?> (("parse_mode"           <=>) <$> parse_mode msg)
  <:?> (("disable_notification" <=>) <$> showBool <$> disable_notification msg)
  <:?> (("reply_to_message_id"  <=>) <$> show <$> reply_to_message_id msg)

simpleMessage :: Int -> String -> SendMessage
simpleMessage cid text
  = SendMessage { chat_id              = cid
                , text                 = text
                , parse_mode           = Nothing
                , disable_notification = Nothing
                , reply_to_message_id  = Nothing
                }

replyMessage :: Int -> String -> Int -> SendMessage
replyMessage cid text replyId
  = simple { reply_to_message_id = Just replyId }
    where simple = simpleMessage cid text

markdownReplyMessage :: Int -> String -> Int -> SendMessage
markdownReplyMessage cid markdown replyId
  = replyMsg { parse_mode = Just "MarkdownV2" }
    where replyMsg = replyMessage cid markdown replyId

showBool True = "true"
showBool False = "false"
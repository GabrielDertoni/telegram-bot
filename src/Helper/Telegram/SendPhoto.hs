module Helper.Telegram.SendPhoto
  ( SendPhoto(..)
  , simplePhoto
  , replyPhoto
  ) where

import qualified Network.URI.Encode as URI

import qualified Helper.Telegram as Telegram
import           Helper.Query
import           Helper.Maybe

-- TODO: Implement reply_markup as well

data SendPhoto
  = SendPhoto { chat_id :: Int
              , photo :: String
              , caption :: Maybe String
              , parse_mode :: Maybe String
              , disable_notification :: Maybe Bool
              , reply_to_message_id :: Maybe Int
              } deriving (Eq, Show)

instance Query SendPhoto where
  getURL pic = (<> sendPhotoQuery pic) <$> Telegram.getEndpointURL "sendPhoto"

sendPhotoQuery :: SendPhoto -> String
sendPhotoQuery pic
    =  ( "chat_id"              <=> (show $ chat_id pic))
  <:>  ( "photo"                <=> photo pic)
  <:?> (("caption"              <=>) <$> URI.encode <$> caption pic)
  <:?> (("parse_mode"           <=>) <$> parse_mode pic)
  <:?> (("disable_notification" <=>) <$> showBool <$> disable_notification pic)
  <:?> (("reply_to_message_id"  <=>) <$> show <$> reply_to_message_id pic)

simplePhoto :: Int -> String -> SendPhoto
simplePhoto cid photoURL
  = SendPhoto { chat_id              = cid
              , photo                = photoURL
              , caption              = Nothing
              , parse_mode           = Nothing
              , disable_notification = Nothing
              , reply_to_message_id  = Nothing
              }

replyPhoto :: Int -> String -> Int -> SendPhoto
replyPhoto cid photoURL replyId
  = simple { reply_to_message_id = Just replyId }
    where simple = simplePhoto cid photoURL
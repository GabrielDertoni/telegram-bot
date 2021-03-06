module Helper.Telegram.Types where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))


data User
    = User { user_id :: Integer
           , is_bot :: Bool
           , first_name :: String
           } deriving(Eq, Show)

instance Aeson.FromJSON User where
    parseJSON (Aeson.Object v) = do
        user_id    <- v .: "id"
        is_bot     <- v .: "is_bot"
        first_name <- v .: "first_name"
        return $
            User { user_id = user_id
                 , is_bot = is_bot
                 , first_name = first_name
                 }

data Chat
    = Chat { chat_id :: Int
           , chat_type :: String
           } deriving(Eq, Show)

instance Aeson.FromJSON Chat where
    parseJSON (Aeson.Object v) = do
        cid   <- v .: "id"
        ctype <- v .: "type"
        return $
            Chat { chat_id = cid
                 , chat_type = ctype
                 }

data MessageEntity
    = MessageEntity { entity_type :: String
                    , entity_offset :: Int
                    , entity_length :: Int
                    } deriving(Eq, Show, Generic)


instance Aeson.FromJSON MessageEntity where
    parseJSON (Aeson.Object v) = do
        entity_type   <- v .: "type"
        entity_offset <- v .: "offset"
        entity_length <- v .: "length"
        return $
            MessageEntity { entity_type   = entity_type
                          , entity_offset = entity_offset
                          , entity_length = entity_length
                          }

data Message
    = Message { message_id :: Int
              , from :: User
              , date :: Int
              , chat :: Chat
              , text :: String
              , entities :: Maybe [MessageEntity]
              , reply_to_message :: Maybe Message
              } deriving(Eq, Show)

getMessageID :: Message -> Int
getMessageID = message_id

getMessageChatID :: Message -> Int
getMessageChatID = chat_id . chat

getMessageIdentifier :: Message -> (Int, Int)
getMessageIdentifier msg = (getMessageChatID msg, getMessageID msg)

getMessageReplyTo :: Message -> Maybe Message
getMessageReplyTo = reply_to_message

getMessageText :: Message -> String
getMessageText = text

instance Aeson.FromJSON Message where
    parseJSON (Aeson.Object v) = do
        message_id <- v .:  "message_id"
        from       <- v .:  "from"
        date       <- v .:  "date"
        chat       <- v .:  "chat"
        text       <- v .:  "text"
        entities   <- v .:? "entities"
        rep_msg    <- v .:? "reply_to_message"
        return $
            Message { message_id = message_id
                 , from = from
                 , date = date
                 , chat = chat
                 , text = text
                 , entities = entities
                 , reply_to_message = rep_msg
                 }

data Update
    = Update { update_id :: Integer
             , message   :: Message
             } deriving(Eq, Show, Generic)


instance Aeson.FromJSON Update where
    parseJSON (Aeson.Object v) = do
        update_id   <- v .: "update_id"
        message     <- v .: "message"
        return $
            Update { update_id = update_id
                   , message = message
                   }

type GetUpdatesResponse = ResponseWrapper [Update]

-- TODO: Finish implementation

data WebHookInfo
  = WebHookInfo { url :: String
                }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data ResponseWrapper a = ResponseWrapper { ok :: Bool
                                         , result :: a
                                         }
  deriving (Eq, Show, Generic, Aeson.FromJSON)
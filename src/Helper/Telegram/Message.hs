module Helper.Telegram.Message where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

import Helper.Telegram.User
import Helper.Telegram.Chat

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
              } deriving(Eq, Show)


instance Aeson.FromJSON Message where
    parseJSON (Aeson.Object v) = do
        message_id <- v .: "message_id"
        from       <- v .: "from"
        date       <- v .: "date"
        chat       <- v .: "chat"
        text       <- v .: "text"
        entities   <- v .:? "entities"
        return $
            Message { message_id = message_id
                 , from = from
                 , date = date
                 , chat = chat
                 , text = text
                 , entities = entities
                 }
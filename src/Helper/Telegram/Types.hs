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


data GetUpdatesResponse
    = GetUpdatesResponse { ok :: Bool
                         , results :: [Update]
                         }
    deriving(Eq, Show, Generic)

instance Aeson.FromJSON GetUpdatesResponse where
    parseJSON (Aeson.Object v) = do
        ok      <- v .: "ok"
        results <- v .: "result"
        return $
            GetUpdatesResponse { ok = ok
                               , results = results
                               }

-- TODO: Finish implementation

data WebHookInfo
  = WebHookInfo { url :: String
                }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)
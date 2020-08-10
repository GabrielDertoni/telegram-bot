module Helper.Telegram.Chat (Chat(..)) where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

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
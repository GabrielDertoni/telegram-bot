module Helper.Telegram.Update where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

import Helper.Telegram.Message

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
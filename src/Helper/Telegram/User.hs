module Helper.Telegram.User (User) where

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
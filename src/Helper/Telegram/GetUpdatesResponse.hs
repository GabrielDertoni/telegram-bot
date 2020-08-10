module Helper.Telegram.GetUpdatesResponse where

import Helper.Telegram.Update

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

data GetUpdatesResponse
    = GetUpdatesResponse { ok :: Bool
                         , results :: [Update]
                         } deriving(Eq, Show, Generic)

instance Aeson.FromJSON GetUpdatesResponse where
    parseJSON (Aeson.Object v) = do
        ok      <- v .: "ok"
        results <- v .: "result"
        return $
            GetUpdatesResponse { ok = ok
                               , results = results
                               }
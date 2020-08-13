module Helper.Telegram.GetUpdates
  ( GetUpdates(..)
  , offsetUpdate
  , getUpdatesQuery
  ) where

import qualified Network.URI.Encode as URI

import qualified Helper.Telegram as Telegram
import           Helper.Query

data GetUpdates
  = GetUpdates { offset :: Maybe Integer
               , limit :: Maybe Int
               , timeout :: Maybe Int
               , allowed_updates :: Maybe [String]
               }

instance Query GetUpdates where
  getURL msg = (<> getUpdatesQuery msg) <$> Telegram.getEndpointURL "getUpdates"

getUpdatesQuery :: GetUpdates -> String
getUpdatesQuery req = "" 
  <:?> (("offset"          <=>) <$> show <$> offset req)
  <:?> (("limit"           <=>) <$> show <$> limit req)
  <:?> (("timeout"         <=>) <$> show <$> timeout req)
  <:?> (("allowed_updates" <=>) <$> show <$> allowed_updates req)

simpleUpdate :: GetUpdates
simpleUpdate = GetUpdates { offset = Nothing
                          , limit = Nothing
                          , timeout = Nothing
                          , allowed_updates = Nothing
                          }

offsetUpdate :: Integer -> GetUpdates
offsetUpdate offset = simple { offset = Just offset }
  where simple = simpleUpdate
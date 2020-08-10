module Helper.Telegram.SetWebhook where

import qualified Helper.Telegram as Telegram
import Helper.Query
import Helper.URL

data SetWebhook
  = SetWebhook { url :: String
               }
  deriving (Eq, Show)

instance Query SetWebhook where
  getURL setWebhook = Telegram.endpointURL "setWebhook" <> ("url" <=> url setWebhook)

simpleWebhook :: String -> SetWebhook
simpleWebhook url = SetWebhook { url = url }
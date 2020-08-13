module Helper.Telegram.SetWebhook where

import qualified Helper.Telegram as Telegram
import Helper.Query

data SetWebhook
  = SetWebhook { url :: String
               }
  deriving (Eq, Show)

instance Query SetWebhook where
  getURL setWebhook = (<> ("url" <=> url setWebhook)) <$> Telegram.getEndpointURL "setWebhook"

simpleWebhook :: String -> SetWebhook
simpleWebhook url = SetWebhook { url = url }
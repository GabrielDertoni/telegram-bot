module Helper.Telegram.WebHookInfo where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

-- TODO: Finish implementation

data WebHookInfo
  = WebHookInfo { url :: String
                } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)
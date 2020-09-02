module Entity.BotInfo
  ( BotInfo(..)
  , defaultInfo
  )
  where

import GHC.Generics (Generic)

data BotInfo
  = BotInfo { bot_name :: String
            , update_offset :: Integer
            , sleep_time :: Double
            , funfact_offset :: Integer
            }
  deriving (Eq, Show, Generic)

defaultInfo :: BotInfo
defaultInfo
  = BotInfo { bot_name = "DertoniBot"
            , update_offset = 1
            , sleep_time = 1
            , funfact_offset = 0
            }
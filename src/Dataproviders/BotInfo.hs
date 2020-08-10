module Dataproviders.BotInfo where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

import Helper.Maybe

data BotInfo
  = BotInfo { bot_name :: String
            , update_offset :: Integer
            , sleep_time :: Double
            } deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

botInfoFile :: FilePath
botInfoFile = "./data.json"

getInfo :: IO BotInfo
getInfo = handle useDefault $ do
            content <- readFile botInfoFile
            return $ (Aeson.decode $ BLU.fromString content) ?? defaultInfo

setInfo :: BotInfo -> IO ()
setInfo info = writeFile botInfoFile $ BLU.toString $ Aeson.encode info

defaultInfo :: BotInfo
defaultInfo = BotInfo { bot_name = "DertoniBot"
                      , update_offset = 1
                      , sleep_time = 1
                      }

useDefault :: IOException -> IO BotInfo
useDefault exception = do print exception
                          setInfo defaultInfo
                          return defaultInfo
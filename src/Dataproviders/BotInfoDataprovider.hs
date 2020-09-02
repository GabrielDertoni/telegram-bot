module Dataproviders.BotInfoDataprovider
  ( BotInfoDataprovider
  , botInfoDataprovider
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Control.Exception

import           Helper.Maybe
import           Entity.BotInfo
import qualified Interface.BotInfo as I

data BotInfoDataprovider
  = BotInfoDataprovider { fname :: String
                        }

instance Aeson.FromJSON BotInfo
instance Aeson.ToJSON BotInfo

instance I.BotInfo BotInfoDataprovider where
  getInfo = getInfo
  setInfo = setInfo
  getIncFunfactInfo = getIncFunfactInfo

botInfoDataprovider :: BotInfoDataprovider
botInfoDataprovider = BotInfoDataprovider { fname = "./assets/data.json" }

getInfo :: BotInfoDataprovider -> IO BotInfo
getInfo provider = handle (useDefault provider) $ do
  content <- readFile $ fname provider
  return $ (Aeson.decode $ BLU.fromString content) ?? defaultInfo

setInfo :: BotInfoDataprovider -> BotInfo -> IO ()
setInfo provider info = writeFile (fname provider) $ BLU.toString $ Aeson.encode info

getIncFunfactInfo :: BotInfoDataprovider -> IO Integer
getIncFunfactInfo provider = do
  info <- getInfo provider
  info `seq` setInfo provider $ info { funfact_offset = funfact_offset info + 1 }
  return $ funfact_offset info

useDefault :: BotInfoDataprovider -> IOException -> IO BotInfo
useDefault provider exception = do
  -- Logger.log $ show exception
  print exception
  setInfo provider defaultInfo
  return defaultInfo
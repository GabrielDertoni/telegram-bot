module Dataproviders.BotInfoDataprovider
  ( BotInfoDataprovider
  -- , getBotInfoDataprovider
  , botInfoDataprovider
  )
  where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Control.Exception
import           Control.Concurrent
import           GHC.IO.Handle
import           System.IO

import           Helper.File
import           Helper.Maybe
import           Entity.BotInfo
import qualified Interface.BotInfo as I

data BotInfoDataprovider
  = BotInfoDataprovider { fname :: String
                        -- , mutexVar :: MVar ()
                        }

instance Aeson.FromJSON BotInfo
instance Aeson.ToJSON BotInfo

instance I.BotInfo BotInfoDataprovider where
  getInfo = getInfo
  setInfo = setInfo
  getIncFunfactInfo = getIncFunfactInfo

botInfoDataprovider :: BotInfoDataprovider
botInfoDataprovider = BotInfoDataprovider { fname = "./assets/data.json" }

{-
getBotInfoDataprovider :: IO BotInfoDataprovider
getBotInfoDataprovider = do
  return BotInfoDataprovider { fname = "./assets/data.json"
                             }
-}

getInfo :: BotInfoDataprovider -> IO BotInfo
getInfo provider = handle (useDefault provider) $ do
  contents <- readFileAsUTF8 $ fname provider
  return $ (Aeson.decode $ BLU.fromString contents) ?? defaultInfo

setInfo :: BotInfoDataprovider -> BotInfo -> IO ()
setInfo provider info = do
  let contents = BLU.toString $ Aeson.encode info
  writeFileAsUTF8 (fname provider) contents

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
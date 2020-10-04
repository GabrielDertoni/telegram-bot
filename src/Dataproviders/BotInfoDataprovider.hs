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
  handle <- openFile (fname provider) ReadMode
  hSetEncoding handle utf8
  -- Aquire a lock on the file resource
  -- hLock handle ExclusiveLock
  content <- hGetContents handle
  return $ (Aeson.decode $ BLU.fromString content) ?? defaultInfo

setInfo :: BotInfoDataprovider -> BotInfo -> IO ()
setInfo provider info = do
  handle <- openFile (fname provider) WriteMode
  hSetEncoding handle utf8
  -- Aquire a lock on the file resource
  -- hLock handle ExclusiveLock
  hPutStr handle $ BLU.toString $ Aeson.encode info

getIncFunfactInfo :: BotInfoDataprovider -> IO Integer
getIncFunfactInfo provider = do
  handle <- openFile (fname provider) ReadWriteMode
  contents <- hGetContents handle
  let info = (Aeson.decode $ BLU.fromString contents) ?? defaultInfo
  let info' = info { funfact_offset = funfact_offset info + 1 }
  hPutStr handle $ BLU.toString $ Aeson.encode info'
  return $ funfact_offset info

useDefault :: BotInfoDataprovider -> IOException -> IO BotInfo
useDefault provider exception = do
  -- Logger.log $ show exception
  print exception
  setInfo provider defaultInfo
  return defaultInfo
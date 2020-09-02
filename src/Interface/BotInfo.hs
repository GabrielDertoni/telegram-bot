module Interface.BotInfo
  ( BotInfo
  , getInfo
  , setInfo
  , getIncFunfactInfo
  )
  where

import qualified Entity.BotInfo as Info

class BotInfo a where
  getInfo :: a -> IO Info.BotInfo
  setInfo :: a -> Info.BotInfo -> IO ()
  getIncFunfactInfo :: a -> IO Integer
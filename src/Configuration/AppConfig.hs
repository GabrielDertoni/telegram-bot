module Configuration.AppConfig
  ( Context(..)
  , getContext
  , logsFilePath
  )
  where

import System.Environment
import Control.Exception (SomeException, handle)

data Context = Production | Development

instance Show Context where
  show Production = "Production"
  show Development = "Development"

defaultContextStr :: String
defaultContextStr = "Production"

getContext :: IO Context
getContext = do ctxt <- handle defaultHandle $ getEnv "CONTEXT"
                case ctxt of
                  "Development" -> return Development
                  _             -> return Production
  where defaultHandle :: SomeException -> IO String
        defaultHandle _ = pure defaultContextStr

logsFilePath :: FilePath
logsFilePath = "./logs.log"
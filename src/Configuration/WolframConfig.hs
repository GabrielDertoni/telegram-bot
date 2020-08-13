module Configuration.WolframConfig where

import System.Environment

getAppId :: IO String
getAppId = getEnv "WOLFRAM_APP_ID"
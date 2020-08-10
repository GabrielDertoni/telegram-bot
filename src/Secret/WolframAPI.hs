module Secret.WolframAPI where

import System.Environment

appId :: String
appId = "XKLTHR-AGUTH5L44Q"

getAppId :: IO String
getAppId = getEnv "WOLFRAM_APP_ID"
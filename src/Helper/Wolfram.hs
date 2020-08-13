module Helper.Wolfram where

import qualified Configuration.WolframConfig as API

baseURL = "https://api.wolframalpha.com/v2/query?"

getBaseURL :: IO String
getBaseURL = ((<>) baseURL) <$> API.getAppId

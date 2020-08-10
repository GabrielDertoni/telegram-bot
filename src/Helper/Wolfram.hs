module Helper.Wolfram where

import Secret.WolframAPI

baseURL :: String
baseURL = "https://api.wolframalpha.com/v2/query?output=json&appid=" <> appId

module Helper.Telegram where

import Configuration.TelegramConfig as API

baseURL :: String
baseURL = "https://api.telegram.org/bot"

getBotURL :: IO String
getBotURL = ((<>) baseURL) <$> API.getApiKey

getEndpointURL :: String -> IO String
getEndpointURL name = (<> "/" <> name <> "?") <$> getBotURL

getUpdatesURL :: IO String
getUpdatesURL = getEndpointURL "getUpdates"
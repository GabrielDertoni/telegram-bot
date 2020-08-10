module Helper.Telegram where

import Secret.TelegramAPI as API

baseURL :: String
baseURL = "https://api.telegram.org/bot"

botURL :: String
botURL = baseURL <> API.apiKey

endpointURL :: String -> String
endpointURL name = botURL <> "/" <> name <> "?"
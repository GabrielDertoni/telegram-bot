module Secret.TelegramAPI where

import System.Environment

apiKey :: String
apiKey = "1345720262:AAHl4J5oxpOGiyLyPG8lh-VVcPnbd87mA9I"

getApiKey :: IO String
getApiKey = getEnv "telegram-api-key"
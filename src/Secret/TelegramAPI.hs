module Secret.TelegramAPI where

import System.Environment

apiKey :: String
apiKey = "1345720262:AAG-7xzBvpbuQ8KAAuRTa3JK00T3_ILnjeo"

getApiKey :: IO String
getApiKey = getEnv "telegram-api-key"
module Configuration.TelegramConfig where

import System.Environment

import Configuration.HerokuConfig as Heroku

getApiKey :: IO String
getApiKey = getEnv "TELEGRAM_API_KEY"

getWebhookURL :: IO String
getWebhookURL = do apiKey  <- getApiKey
                   projURL <- Heroku.projectURL
                   return $ projURL <> apiKey <> "/update"

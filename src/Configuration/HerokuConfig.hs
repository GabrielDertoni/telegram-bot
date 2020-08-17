module Configuration.HerokuConfig where

import System.Environment

projectURL :: IO String
-- projectURL = "https://wolfram-telegram-bot.herokuapp.com/"
projectURL = getEnv "PROJECT_URL"
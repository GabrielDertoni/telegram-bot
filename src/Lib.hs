module Lib (run) where

import qualified Data.Aeson as Aeson
import Data.Maybe
import Web.Scotty
import System.Environment
import Control.Exception (IOException, catch)
import Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)

import Helper.Telegram.Types
import Controller.SetWebhook
import API.UpdateHandler
import API.GetDataHandler
import API.GetLogsHandler
import Helper.Telegram
import Dataproviders.BotInfo
import Helper.Telegram.SetWebhook   as Telegram
import Configuration.TelegramConfig as Telegram

useDefaultPort :: IOException -> IO String
useDefaultPort _ = do putStrLn "No environment variable set for port, using default 8000"
                      return "8000"

run :: IO ()
run = do
    -- Will try to load the environment variables from .env file
    -- if no such file is found, it will do nothing and may fail later.
    onMissingFile (loadFile defaultConfig) $ return []
    webhookURL <- Telegram.getWebhookURL
    setWebhook $ Telegram.simpleWebhook webhookURL
    port <- catch (getEnv "PORT") useDefaultPort
    updateHandler  <- getUpdateHandler
    getDataHandler <- getGetDataHandler
    getLogsHandler <- getGetLogsHandler
    scotty (read port) $ do
      updateHandler
      getDataHandler
      getLogsHandler

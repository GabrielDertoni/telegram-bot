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
import Helper.Telegram
import Dataproviders.BotInfo
import Helper.Telegram.SetWebhook as Telegram
import Configuration.TelegramConfig as Telegram

{-
getLastUpdateId :: Telegram.GetUpdatesResponse -> Maybe Integer
getLastUpdateId resp = case Telegram.results resp of
                        []      -> Nothing
                        results -> Just $ Telegram.update_id $ last results

updateOffset :: BotInfo -> Integer -> IO ()
updateOffset info off = setInfo $ info { update_offset = off + 1 }

updateInfo :: Telegram.GetUpdatesResponse -> BotInfo -> IO ()
updateInfo resp info = maybe (return ()) (updateOffset info) $ getLastUpdateId resp

handleException :: SomeException -> IO ()
handleException = print
-}

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
    scotty (read port) $ do
      updateHandler
      getDataHandler



-- run = forever $ handle handleException $ do
--     info <- getInfo
--     response <- simpleHttp $ getURL $ offsetUpdate $ update_offset info
--     case Aeson.eitherDecode response of
--         Right decoded -> do updateInfo decoded info
--                             main_entrypoint $ results decoded
                            
--         Left err      -> fail err
    
--     putStrLn "Just read the updates..."
--     sleep $ sleep_time info



-- run = do setWebhook $ Telegram.simpleWebhook Telegram.webhookURL
--          startServer [ updateHandler
--                      , getDataHandler
--                      ]



-- run = do
--     response <- simpleHttp url
--     either fail (main_entrypoint . results) $ Aeson.eitherDecode response
--     putStrLn "Finished..."

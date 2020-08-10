module Lib (run) where

import Zero.Server
import System.Time.Extra
import Control.Concurrent
import Control.Monad
import qualified Data.Aeson as Aeson
import Network.HTTP.Conduit
import Data.Maybe
import Control.Exception

import Helper.Telegram.GetUpdatesResponse
import Controller.AskEntrypoint
import Controller.SetWebhook
import API.UpdateHandler
import API.GetDataHandler
import Helper.Telegram
import Helper.Query
import Helper.Telegram.GetUpdates
import Dataproviders.BotInfo
import Helper.Telegram.GetUpdatesResponse as Telegram
import Helper.Telegram.Update as Telegram
import Helper.Telegram.Message as Telegram
import Helper.Telegram.SetWebhook as Telegram
import Configuration.TelegramConfig as Telegram

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

run :: IO ()
-- run = forever $ handle handleException $ do
--     info <- getInfo
--     response <- simpleHttp $ getURL $ offsetUpdate $ update_offset info
--     case Aeson.eitherDecode response of
--         Right decoded -> do updateInfo decoded info
--                             main_entrypoint $ results decoded
                            
--         Left err      -> fail err
    
--     putStrLn "Just read the updates..."
--     sleep $ sleep_time info
run = do setWebhook $ Telegram.simpleWebhook Telegram.webhookURL
         startServer [ updateHandler
                     , getDataHandler
                     ]
-- run = do
--     response <- simpleHttp url
--     either fail (main_entrypoint . results) $ Aeson.eitherDecode response
--     putStrLn "Finished..."

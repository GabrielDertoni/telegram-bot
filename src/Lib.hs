module Lib (run) where

import Zero.Server
import System.Time.Extra
import Control.Concurrent
import Control.Monad
import qualified Data.Aeson as Aeson
import Network.HTTP.Conduit
import Data.Maybe

import Helper.Telegram.GetUpdatesResponse
import Controller.AskEntrypoint
import API.UpdateHandler
import Helper.Telegram
import Helper.Query
import Helper.Telegram.GetUpdates
import Dataproviders.BotInfo
import Helper.Telegram.GetUpdatesResponse as Telegram
import Helper.Telegram.Update as Telegram
import Helper.Telegram.Message as Telegram
import Control.Exception

updatesURL = "https://api.telegram.org/bot1345720262:AAHl4J5oxpOGiyLyPG8lh-VVcPnbd87mA9I/getUpdates"

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
run = forever $ handle handleException $ do
    info <- getInfo
    response <- simpleHttp $ getURL $ offsetUpdate $ update_offset info
    case Aeson.eitherDecode response of
        Right decoded -> do main_entrypoint $ results decoded
                            updateInfo decoded info

        Left err      -> fail err
    
    putStrLn "Just read the updates..."
    sleep $ sleep_time info
-- run = startServer [
--        updateHandler
--     ]
-- run = do
--     response <- simpleHttp url
--     either fail (main_entrypoint . results) $ Aeson.eitherDecode response
--     putStrLn "Finished..."

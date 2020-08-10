module Lib where

import Network.HTTP.Conduit
import qualified Data.Aeson as Aeson

import Helper.Telegram.GetUpdatesResponse
import Controller.AskEntrypoint

url = "https://api.telegram.org/bot1345720262:AAHl4J5oxpOGiyLyPG8lh-VVcPnbd87mA9I/getUpdates"
baseURL = "https://api.telegram.org/bot1345720262:AAHl4J5oxpOGiyLyPG8lh-VVcPnbd87mA9I/sendMessage?"

someFunc :: IO ()
someFunc = do
    response <- simpleHttp url
    either fail (main_entrypoint . results) $ Aeson.eitherDecode response
    putStrLn "Finished..."

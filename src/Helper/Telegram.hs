module Helper.Telegram where

import qualified Helper.Telegram.Types        as Telegram
import qualified Configuration.TelegramConfig as API

baseURL :: String
baseURL = "https://api.telegram.org/bot"

getBotURL :: IO String
getBotURL = ((<>) baseURL) <$> API.getApiKey

getEndpointURL :: String -> IO String
getEndpointURL name = (<> "/" <> name <> "?") <$> getBotURL

getUpdatesURL :: IO String
getUpdatesURL = getEndpointURL "getUpdates"

getCommand :: Telegram.Message -> Maybe (String, String)
getCommand msg = do entity <- head <$> Telegram.entities msg
                    let text    = Telegram.text msg
                    let len     = Telegram.entity_length entity
                    let off     = Telegram.entity_offset entity
                    let after   = drop (len + off) text
                    let command = take len $ drop off text
                    return (command, after)
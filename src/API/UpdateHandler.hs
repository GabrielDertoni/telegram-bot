module API.UpdateHandler where

import Zero.Server

import Secret.TelegramAPI as Telegram
import Helper.Telegram.Update
import Controller.AskEntrypoint

updateCallback :: Request -> IO Response
updateCallback req = case decodeRequest req of
                      Right updates -> do main_entrypoint updates
                                          return $ stringResponse "True"
                      Left err      -> fail err

decodeRequest :: Request -> Either String [Update]
decodeRequest = decodeJson . requestBody

updateHandler :: Handler
updateHandler = effectfulHandler POST ("/" <> Telegram.apiKey <> "/update") updateCallback
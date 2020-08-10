module API.UpdateHandler where

import Control.Exception
import qualified Zero.Server as Server

import Secret.TelegramAPI as Telegram
import Helper.Telegram.Update
import Controller.AskEntrypoint

updateCallback :: Server.Request -> IO Server.Response
updateCallback req = case decodeRequest req of
                      Right update -> do handle handleEntrypoint $ assign_entrypoint update
                                         return $ Server.stringResponse "Ok"
                      Left err      -> fail err
  where handleEntrypoint :: IOException -> IO ()
        handleEntrypoint = print

printCallback :: Server.Request -> IO Server.Response
printCallback req = do print $ Server.requestBody req
                       return $ Server.stringResponse "True"

decodeRequest :: Server.Request -> Either String Update
decodeRequest = Server.decodeJson . Server.requestBody

updateHandler :: Server.Handler
updateHandler = Server.effectfulHandler Server.POST ("/" <> Telegram.apiKey <> "/update") updateCallback
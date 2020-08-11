module API.UpdateHandler where

import Control.Exception
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class
import Data.ByteString.Lazy.UTF8 (ByteString)

import Secret.TelegramAPI as Telegram
import Helper.Telegram.Update
import Controller.AskEntrypoint

updateCallback :: Scotty.ActionM ()
updateCallback = do
      req <- Scotty.body
      case decodeRequest req of
        Right update -> do liftIO $ assign_entrypoint update
                           Scotty.text "Ok"
        Left err     -> fail err

printCallback :: Scotty.ActionM ()
printCallback = do req <- Scotty.body
                   liftIO $ print req
                   Scotty.text "True"

decodeRequest :: ByteString -> Either String Update
decodeRequest = Aeson.eitherDecode

updateHandlerPath = "/" <> Telegram.apiKey <> "/update"

updateHandler :: Scotty.ScottyM ()
updateHandler = Scotty.post (Scotty.capture updateHandlerPath) updateCallback
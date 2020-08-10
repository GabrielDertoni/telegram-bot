module API.GetDataHandler where

import qualified Zero.Server as Server

import Dataproviders.BotInfo
import qualified Secret.TelegramAPI as Telegram

getDataCallback :: Server.Request -> IO Server.Response
getDataCallback _ = Server.jsonResponse <$> getInfo

getDataHandler :: Server.Handler
getDataHandler = Server.effectfulHandler Server.GET ("/" <> Telegram.apiKey <> "/data") getDataCallback
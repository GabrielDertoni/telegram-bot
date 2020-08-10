module Configuration.TelegramConfig where

import Configuration.HerokuConfig as Heroku
import Helper.Telegram            as Telegram
import Secret.TelegramAPI         as Telegram

webhookURL = Heroku.projectURL <> Telegram.apiKey <> "/update"
updatesURL = Telegram.endpointURL "getUpdates"
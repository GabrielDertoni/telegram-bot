module API.UpdateHandler where

import           Text.Printf
import           Control.Exception
import qualified Web.Scotty                     as Scotty
import qualified Data.Aeson                     as Aeson
import           Control.Monad.IO.Class
import qualified Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TChan   as STM
import           Data.ByteString.Lazy.UTF8 (ByteString, toString)

import qualified Controller.SendMessage         as Send
import qualified Configuration.TelegramConfig   as Telegram
import qualified Helper.Telegram.Types          as Telegram
import qualified Helper.Telegram.SendMessage    as Method
import qualified Helper.Telegram                as Telegram
import qualified Entity.Question                as Q
import qualified Entity.Message                 as M
import qualified Entity.Answer                  as A
import qualified Entity.Command                 as Cmd
import qualified Dataproviders.Logger           as Logger
import qualified Configuration.GlobalConfig     as Cfg
import           Interface.Command
import qualified Interface.GetAnswer            as I
import qualified Interface.GetIDK               as I
import qualified Interface.GetFunfact           as I
import qualified Interface.BotInfo              as I
import qualified UseCase.IDK                    as Use
import qualified UseCase.Funfact                as Use
import qualified UseCase.AskWolfram             as Use
import qualified UseCase.InterpretBrainfuck     as Use
import qualified UseCase.Start                  as Use
import qualified UseCase.Help                   as Use
import qualified UseCase.Settings               as Use


allCommands :: (I.GetAnswer c, I.GetIDK c, I.GetFunfact c, I.BotInfo c) => CommandM c ()
allCommands = do
  Use.startCommand
  Use.helpCommand
  Use.settingsCommand
  Use.wolframCommand
  Use.brainfuckCommand
  Use.idkCommand
  -- Use.funfactCommand

handl :: IOException -> IO ()
handl e = Logger.log $ show e

updateCallback :: STM.TChan Cmd.Command -> Scotty.ActionM ()
updateCallback broadChan = do
  req <- Scotty.body
  case decodeRequest req of
    Right update -> do liftIO $ STM.atomically $ STM.writeTChan broadChan $ updateToCommand update
                       liftIO $ handle handl $ sequenceCommands allCommands (Cfg.globalConfig, updateToCommand update, broadChan)
                       Scotty.text "true"

    Left err     -> do liftIO $ Logger.log $ printf "Failed with error: %s\nwhile parsing:\n%s" err $ toString req
                       Scotty.text "true"

updateToCommand :: Telegram.Update -> Cmd.Command
updateToCommand update
  = case cmd of
      Just (c, a) -> Cmd.simple cid c a
      Nothing     -> case Telegram.getMessageReplyTo msg of
                      Just repl -> Cmd.reply cid txt $ Telegram.getMessageIdentifier repl
                      Nothing   -> Cmd.mension cid txt
  where cmd = Telegram.getCommand msg
        txt = Telegram.getMessageText msg
        cid = Telegram.getMessageIdentifier msg
        msg = Telegram.message update

decodeRequest :: ByteString -> Either String Telegram.Update
decodeRequest = Aeson.eitherDecode

getUpdateHandlerPath :: IO String
getUpdateHandlerPath = do apiKey <- Telegram.getApiKey
                          return $ "/" <> apiKey <> "/update"

getUpdateHandler :: IO (Scotty.ScottyM ())
getUpdateHandler = do path <- getUpdateHandlerPath
                      broadChan <- STM.newBroadcastTChanIO
                      return $ Scotty.post (Scotty.capture path) $ updateCallback broadChan
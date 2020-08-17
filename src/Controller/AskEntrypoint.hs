module Controller.AskEntrypoint where

import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Maybe
import qualified Data.Aeson                    as Aeson
import           Control.Concurrent.Async
import           Control.Exception
import           Text.Printf
import qualified Control.Concurrent.STM        as STM
import qualified Control.Concurrent.STM.TChan  as STM
import           Control.Monad.IO.Class (liftIO)

import qualified Helper.Telegram.Types         as Telegram
import qualified Helper.Telegram.SendMessage   as Telegram (markdownReplyMessage, replyMessage, SendMessage)
import           UseCase.Ask
import           Dataproviders.AskDataprovider
import qualified Entity.Question               as Q
import qualified Entity.Message                as M
import qualified Entity.Answer                 as A
import qualified Interface.ToMarkdown          as I
import           Interface.Entrypoint
import qualified Controller.SendMessage        as Send

-- TODO: Change folder name from "Controller" to "Entrypoint" or "Commands"

answerToMessage :: A.Answer -> M.Message
answerToMessage ans
  = maybe (M.simpleMessage $ A.text ans) M.imageMessage (A.image ans)

wolframCommand :: EntrypointM ()
wolframCommand = command "/wolfram" $ do
  content <- getContentAfterCommand
  message <- getCallerMessage
  let question = Q.Question content
  answer <- liftIO $ ask askDataproviderImplementation question
  let ansMsg = answerToMessage answer
  liftIO $ Send.sendReplyTo message ansMsg
  return Nothing
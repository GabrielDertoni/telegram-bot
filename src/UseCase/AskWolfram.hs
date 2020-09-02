module UseCase.AskWolfram
  ( wolframCommand
  )
  where

import           Control.Monad.IO.Class (liftIO)

import qualified Controller.SendMessage            as Send
import           Dataproviders.WolframDataprovider
import qualified Entity.Question                   as Q
import qualified Entity.Message                    as Msg
import qualified Entity.Answer                     as A
import qualified Interface.GetAnswer               as I
import           Interface.Command

answerToText :: A.Answer -> String
answerToText ans
  = maybe (A.text ans) id (A.image ans)

wolframCommand :: I.GetAnswer c => CommandM c ()
wolframCommand = command "/wolfram" $ do
  cmd <- getCallerCommand
  content <- getContentAfterCommand
  cfg <- getConfig
  let question = Q.Question content
  answer <- liftIO $ I.getAnswer cfg question
  replyText $ answerToText answer
  return Nothing
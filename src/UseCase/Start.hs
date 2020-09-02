module UseCase.Start
  ( startCommand
  )
  where

import Controller.SendMessage
import Interface.Command

startCommand :: CommandM c ()
startCommand = command "/start" $ do
  replyText "Olá você(s)"
  return Nothing
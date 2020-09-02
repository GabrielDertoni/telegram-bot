module UseCase.Settings
  ( settingsCommand
  )
  where

import Controller.SendMessage
import Interface.Command

settingsCommand :: CommandM c ()
settingsCommand = command "/settings" $ do
  replyText "Nenhuma configuração disponível."
  return Nothing
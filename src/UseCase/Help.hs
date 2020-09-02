module UseCase.Help
  ( helpCommand
  )
  where

import Controller.SendMessage
import Interface.Command

helpMsg :: String
helpMsg = "DertoniBot \n\
\ Esse bot pode fazer várias coisas diferentes:\n\
\ /wolfram <problema matemático> - Responde as etapas para a resolução do problema (wolfram alpha API).\n\
\ /brainfuck <código em brainfuck> - Roda o código em brainfuck e responde com resultado impresso.\n\
\ /idk - \"Eu não sei\" em algum idioma aleatório.\n\
\ /funfact - Fátos aleatórios sobre a vida, universo e tudo mais."

helpCommand :: CommandM c ()
helpCommand = command "/help" $ do
  replyText helpMsg
  return Nothing
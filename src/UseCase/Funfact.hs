module UseCase.Funfact where

import           Control.Monad.IO.Class (liftIO)
import           System.Random
import           Controller.SendMessage

import qualified Interface.BotInfo     as I
import qualified Interface.GetFunfact  as I
import           Interface.Command

funfactCommand :: (I.GetFunfact c, I.BotInfo c) => CommandM c ()
funfactCommand = command "/funfact" $ do
  cfg <- getConfig
  fun <- liftIO $ I.getFunfact cfg
  replyText fun
  return Nothing
module UseCase.IDK where

import           Control.Monad.IO.Class (liftIO)
import           Controller.SendMessage

import           Interface.Command
import qualified Interface.GetIDK as I

idkCommand :: I.GetIDK c => CommandM c ()
idkCommand = command "/idk" $ do
  cfg <- getConfig
  idk <- liftIO $ I.getIDK cfg
  replyText idk
  return Nothing

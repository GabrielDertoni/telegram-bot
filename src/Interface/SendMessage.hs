module Interface.SendMessage 
  ( SendMessage
  , sendMessage
  )
  where

import qualified Entity.Message as M
import           Entity.Command

class SendMessage m where
  sendMessage :: M.Message -> m MessageId
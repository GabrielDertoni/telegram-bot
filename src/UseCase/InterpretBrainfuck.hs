module UseCase.InterpretBrainfuck where

import Data.Time
import Text.Printf
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

-- import           InterpretBrainfuck.Types
import           Brainfuck.Execution
import           Brainfuck.ProtoOS
import qualified Entity.Message        as M
import qualified Helper.Telegram.Types as Telegram
import qualified Helper.Telegram.SendMessage as Send
import qualified Controller.SendMessage as Send
import qualified Controller.InterpretBrainfuckEntrypoint as BF

interpret :: ExecuteM () -> IO M.Message
interpret program = do
  env <- defaultEnv
  res <- execute program env
  case res of
    Left err -> return $ M.simpleMessage $ printf "Error(s):\n%s" err
    Right (_, env) -> return $ M.simpleMessage $ show $ memory env
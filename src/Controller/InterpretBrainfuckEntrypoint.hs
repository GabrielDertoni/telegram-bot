module Controller.InterpretBrainfuckEntrypoint where

import Data.Time
import Text.Printf
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

-- import           InterpretBrainfuck.Types
import           Brainfuck.Execution
import           Brainfuck.ProtoOS
import           Brainfuck.Compiler
import qualified Entity.Message              as M
import qualified Helper.Telegram.Types       as Telegram
import qualified Helper.Telegram.SendMessage as Send
import qualified Controller.SendMessage      as Send
import qualified Interface.Entrypoint        as I

readFromChannel :: TChan Telegram.Update -> IO String
readFromChannel chan
  = do update <- atomically $ readTChan chan
       return $ Telegram.text $ Telegram.message update

sendInputRequestMessage :: Int -> IO Telegram.Message
sendInputRequestMessage cid
  = Send.sendMessage $ Send.simpleMessage cid "Input required..."

waitForResponseTo :: Int -> Int -> TChan Telegram.Update -> IO Telegram.Message
waitForResponseTo cid mid chan
  = do update <- atomically $ readTChan chan
       let rpl_msg = Telegram.reply_to_message $ Telegram.message $ update
       case rpl_msg of
         Nothing  -> waitForResponseTo cid mid chan
         Just msg -> if Telegram.message_id msg == mid
                      then return msg
                      else waitForResponseTo cid mid chan

getContentFromWaitResponse :: Int -> Int -> TChan Telegram.Update -> IO String
getContentFromWaitResponse cid mid chan
  = do msg <- waitForResponseTo cid mid chan
       return $ Telegram.text msg

readFn :: Int -> TChan Telegram.Update -> TVar (Maybe Telegram.Message) -> IO String
readFn cid chan last_msg
  = do maybe_msg <- atomically $ readTVar last_msg
       case maybe_msg of
         Nothing      -> do inp_msg <- sendInputRequestMessage cid
                            getContentFromWaitResponse cid (Telegram.message_id inp_msg) chan

         Just inp_msg -> getContentFromWaitResponse cid (Telegram.message_id inp_msg) chan

writeFn :: Int -> TChan Telegram.Update -> TVar (Maybe Telegram.Message) -> String -> IO ()
writeFn cid chan last_msg out
  = do msg <- Send.sendMessage $ Send.simpleMessage cid out
       atomically $ modifyTVar last_msg (\_ -> Just msg)

customSystem :: Int -> TChan Telegram.Update -> TChan Telegram.Update -> TVar (Maybe Telegram.Message) -> IO System
customSystem cid ichan ochan last_msg
  = do ds <- defaultSystem
       return ds { read_fn  = readFn  cid ochan last_msg
                 , write_fn = writeFn cid ichan last_msg
                 }

customEnvironment :: Int -> TChan Telegram.Update -> TVar (Maybe Telegram.Message) -> IO Environment
customEnvironment cid chan last_msg
  = do ichan <- atomically $ dupTChan chan
       ochan <- atomically $ dupTChan chan
       sys   <- customSystem cid ichan ochan last_msg
       env   <- defaultEnv
       return env { system = sys }

simpleSystem :: TVar String -> IO System
simpleSystem tvar = do ds <- defaultSystem
                       return ds { read_fn = return "a"
                                 , write_fn = \str -> atomically $ modifyTVar tvar (++ str)
                                 }

simpleEnv :: TVar String -> IO Environment
simpleEnv tvar = do sys <- simpleSystem tvar
                    env <- defaultEnv
                    return env { system = sys }

simpleBrainfuckEntrypoint :: I.BotEntrypoint
simpleBrainfuckEntrypoint program
  = do case parseInstructions program of
        Left err        -> return $ M.simpleMessage err
        Right instructs -> runBF instructs

  where runBF :: [Instruction] -> IO M.Message
        runBF instructs = do
          tvar <- atomically $ newTVar ""
          env <- simpleEnv tvar
          res <- execute (interpret instructs) env
          case res of
            Left err     -> return $ M.simpleMessage err
            Right (_, _) -> do out <- atomically $ readTVar tvar
                               if length out == 0
                                 then return $ M.simpleMessage "(no output)"
                                 else return $ M.simpleMessage out

-- brainfuckEntrypoint :: String -> Telegram.Update -> IO M.Message
-- brainfuckEntrypoint program update
--   = do case parseInstructions program of
--         Left err   -> return $ M.simpleMessage err
--         Right inst -> do forkIO $ runBF (Telegram.message update) instructs
--                          return $ M.simpleMessage "Executing your code..."
  
--   where runBF :: Telegram.Message -> [Instruction] -> IO ()
--         runBF msg inst = do
--           last_msg  <- atomically $ newTVar msg
--           broadChan <- newBroadcastTChanIO
--           inpChan   <- atomically $ dupChan broadChan
--           outChan   <- atomically $ dupChan broadChan

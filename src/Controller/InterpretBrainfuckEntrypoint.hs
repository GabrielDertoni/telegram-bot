module Controller.InterpretBrainfuckEntrypoint where

import Data.Time
import Data.Maybe
import Text.Printf
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad (when)
import Control.Monad.IO.Class

-- import           InterpretBrainfuck.Types
import           Brainfuck.Execution
import           Brainfuck.ProtoOS
import           Brainfuck.Compiler
import qualified Entity.Message                  as M
import qualified Helper.Telegram                 as Telegram
import qualified Helper.Telegram.Types           as Telegram
import qualified Helper.Telegram.SendMessage     as Send
import qualified Helper.Telegram.EditMessageText as Edit
import qualified Controller.SendMessage          as Send
import qualified Interface.Entrypoint            as I
import           Helper.Maybe ((??))

readFromChannel :: TChan Telegram.Update -> IO String
readFromChannel chan
  = do update <- atomically $ readTChan chan
       return $ Telegram.text $ Telegram.message update

sendInputRequestMessage :: Int -> IO Telegram.Message
sendInputRequestMessage cid
  = Send.sendMessage $ Send.simpleMessage cid "(input required)"

waitForResponseTo :: Int -> Int -> TChan Telegram.Update -> IO Telegram.Message
waitForResponseTo cid mid chan
  = do putStrLn $ "Waiting for response to " <> show mid
       update <- atomically $ readTChan chan
       let msg = Telegram.message update
       let may_rpl_msg = Telegram.reply_to_message $ Telegram.message $ update
       case may_rpl_msg of
         Nothing       -> do putStrLn "Message is not responding to another message"
                             waitForResponseTo cid mid chan
         Just repl_msg -> if Telegram.message_id repl_msg == mid
                            then do
                              putStrLn "Found my response :)"
                              return msg
                            else do
                              putStrLn $ "Found reply to " <> show (Telegram.message_id repl_msg) <> " but was searching for reply to " <> show mid
                              waitForResponseTo cid mid chan

waitForResponseToOneOf :: Int -> [Int] -> TChan Telegram.Update -> IO Telegram.Message
waitForResponseToOneOf cid mids chan
  = do update <- atomically $ readTChan chan
       case Telegram.reply_to_message $ Telegram.message $ update of
         Nothing       -> waitForResponseToOneOf cid mids chan
         Just repl_msg -> if Telegram.message_id repl_msg `elem` mids
                            then return $ Telegram.message update
                            else waitForResponseToOneOf cid mids chan

getContentFromWaitResponse :: Int -> Int -> TChan Telegram.Update -> IO String
getContentFromWaitResponse cid mid chan
  = do msg <- waitForResponseTo cid mid chan
       return $ Telegram.text msg

readFn :: Int -> Int -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO String
readFn cid prog_mid chan last_msgs
  = do maybe_msg <- atomically $ readTVar last_msgs
       msg <- case maybe_msg of
                []       -> do inp_msg <- sendInputRequestMessage cid
                               atomically $ modifyTVar last_msgs (\lst -> inp_msg:lst)
                               waitForResponseToOneOf cid [Telegram.message_id inp_msg] chan

                inp_msgs -> waitForResponseToOneOf cid (Telegram.message_id <$> inp_msgs) chan
       
       atomically $ modifyTVar last_msgs (\lst -> msg:lst)
       let content = Telegram.text msg
       when (content == "/kill") $ fail "Program terminated by user"
       return content

writeFn :: Int -> Int -> TChan Telegram.Update -> TVar [Telegram.Message] -> String -> IO ()
writeFn cid prog_mid chan last_msgs out
  = do mid <- atomically $ getMayMid
       msg <- Send.sendMessage $ Send.replyMessage cid out (mid ?? prog_mid)
       atomically $ modifyTVar last_msgs (\lst -> msg:lst)
  where getMayMid :: STM (Maybe Int)
        getMayMid = do val <- readTVar last_msgs
                       return $ Telegram.message_id <$> listToMaybe val

customSystem :: Int -> Int -> TChan Telegram.Update -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO System
customSystem cid prog_mid ichan ochan last_msgs
  = do ds <- defaultSystem
       return ds { read_fn  = readFn  cid prog_mid ochan last_msgs
                 , write_fn = writeFn cid prog_mid ichan last_msgs
                 }

customEnvironment :: Int -> Int -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO Environment
customEnvironment cid prog_mid chan last_msgs
  = do ichan <- atomically $ dupTChan chan
       ochan <- atomically $ dupTChan chan
       sys   <- customSystem cid prog_mid ichan ochan last_msgs
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

brainfuckEntrypoint :: String -> Telegram.Update -> TChan Telegram.Update -> IO M.Message
brainfuckEntrypoint program update broadChan
  = do case parseInstructions program of
        Left err   -> return $ M.simpleMessage err
        Right []   -> return $ M.simpleMessage "No instructions in program"
        Right inst -> do forkIO $ runBF (Telegram.message update) inst
                         return $ M.simpleMessage $ "Executing code. PID: " <> (show $ Telegram.message_id $ Telegram.message update)
  
  where runBF :: Telegram.Message -> [Instruction] -> IO ()
        runBF msg inst = do
          let cid = Telegram.chat_id $ Telegram.chat msg
          let mid = Telegram.message_id msg
          last_msgs <- atomically $ newTVar []
          env       <- customEnvironment cid mid broadChan last_msgs
          res       <- execute (interpret inst) env
          case res of
            Left err     -> Send.sendMessage $ Send.replyMessage cid ("Error(s):\n" <> err) mid
            Right (_, _) -> Send.sendMessage $ Send.replyMessage cid "Finished execution" mid
          
          return ()
          

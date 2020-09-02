module UseCase.InterpretBrainfuck
  ( brainfuckCommand
  )
  where

import           Data.Time
import           Data.Maybe
import           Text.Printf
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan as STM
import           Control.Concurrent.STM.TVar
import           Control.Monad (when)
import           Control.Monad.IO.Class

import qualified Controller.SendMessage       as Send
import           Brainfuck.Execution
import           Brainfuck.ProtoOS
import           Brainfuck.Instructions
import           Brainfuck.Compiler        
import qualified Entity.Message               as Msg
import qualified Entity.Command               as Cmd
import qualified Interface.SendMessage        as I
import           Helper.Maybe ((??))
import           Interface.Command


waitForResponseToOneOf :: [Cmd.MessageId] -> TChan Cmd.Command -> IO Cmd.Command
waitForResponseToOneOf msgs chan
  = do cmd <- atomically $ readTChan chan
       case Cmd.getReplyingTo cmd of
         Nothing       -> waitForResponseToOneOf msgs chan
         Just repl_cmd -> if repl_cmd `elem` msgs
                            then return cmd
                            else waitForResponseToOneOf msgs chan

readFn :: Cmd.MessageId -> Cmd.Command -> TChan Cmd.Command -> TVar [Cmd.MessageId] -> IO String
readFn edit_msg prog_cmd chan last_msgs
  = do maybe_msg <- atomically $ readTVar last_msgs
       cmd <- case maybe_msg of
                []       -> do inp_msg <- I.sendMessage $ Msg.editText edit_msg "(input required)"
                               atomically $ modifyTVar last_msgs (\lst -> inp_msg:lst)
                               waitForResponseToOneOf [inp_msg] chan

                inp_msgs -> waitForResponseToOneOf inp_msgs chan
       
       atomically $ modifyTVar last_msgs (\lst -> Cmd.getId cmd : lst)
       liftIO $ print cmd
       when (maybe False (== "/kill") $ Cmd.getCommand cmd) $ fail "Program terminated by user"
       return $ Cmd.getContent cmd

writeFn :: Cmd.MessageId -> Cmd.Command -> TChan Cmd.Command -> TVar [Cmd.MessageId] -> String -> IO ()
writeFn edit_msg prog_cmd chan last_msgs out
  = do mid <- atomically $ getMayMid
       msg <- I.sendMessage $ Msg.reply (mid ?? Cmd.getId prog_cmd) out
       atomically $ modifyTVar last_msgs (\lst -> msg:lst)
  where getMayMid :: STM (Maybe Cmd.MessageId)
        getMayMid = do val <- readTVar last_msgs
                       return $ listToMaybe val

customSystem :: Cmd.MessageId -> Cmd.Command -> TChan Cmd.Command -> TChan Cmd.Command -> TVar [Cmd.MessageId] -> IO System
customSystem edit_msg prog_cmd ichan ochan last_msgs
  = do ds <- defaultSystem
       return ds { read_fn  = readFn  edit_msg prog_cmd ochan last_msgs
                 , write_fn = writeFn edit_msg prog_cmd ichan last_msgs
                 , time_limit = 60
                 }

customEnvironment :: Cmd.MessageId -> Cmd.Command -> TChan Cmd.Command -> TVar [Cmd.MessageId] -> IO Env
customEnvironment edit_msg prog_cmd chan last_msgs
  = do ichan <- atomically $ dupTChan chan
       ochan <- atomically $ dupTChan chan
       sys   <- customSystem edit_msg prog_cmd ichan ochan last_msgs
       env   <- defaultEnv
       return env { system = sys }

brainfuckCommand :: CommandM c ()
brainfuckCommand = command "/brainfuck" $ do
  cmd     <- getCallerCommand
  chan    <- getCommandChannel
  program <- getContentAfterCommand
  cfg     <- getConfig
  case parseInstructions program of
    Left err   -> replyText err *> return Nothing
    Right []   -> replyText "No instructions in program." *> return Nothing
    Right inst -> do replMsg <- replyText "Executing your code..."
                     liftIO $ forkIO $ runBF replMsg chan cmd inst
                     return Nothing

  where runBF :: Cmd.MessageId -> TChan Cmd.Command -> Cmd.Command -> [Instruction] -> IO ()
        runBF edit_msg chan cmd inst = do
          last_msgs <- atomically $ newTVar []
          env       <- customEnvironment edit_msg cmd chan last_msgs
          res       <- execute (interpret inst) env
          case res of
            Left err     -> I.sendMessage $ Msg.editText edit_msg $ "Error(s):\n" <> err
            Right (_, _) -> I.sendMessage $ Msg.editText edit_msg "Finished execution"
          
          return ()
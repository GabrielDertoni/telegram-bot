module Controller.BrainfuckEntrypoint where

import Data.Time
import Data.Maybe
import Text.Printf
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad (when)
import Control.Monad.IO.Class

import           Brainfuck.Execution
import           Brainfuck.ProtoOS
import           Brainfuck.Compiler
import qualified Entity.Message                  as M
import qualified Helper.Telegram                 as Telegram
import qualified Helper.Telegram.Types           as Telegram
import qualified Helper.Telegram.SendMessage     as Send
import qualified Helper.Telegram.EditMessageText as Edit
import qualified Controller.SendMessage          as Send
import           Helper.Maybe ((??))
import           Interface.Entrypoint

readFromChannel :: TChan Telegram.Update -> IO String
readFromChannel chan
  = do update <- atomically $ readTChan chan
       return $ Telegram.text $ Telegram.message update

waitForResponseTo :: Int -> Int -> TChan Telegram.Update -> IO Telegram.Message
waitForResponseTo cid mid chan
  = do update <- atomically $ readTChan chan
       let msg = Telegram.message update
       let may_rpl_msg = Telegram.reply_to_message $ Telegram.message $ update
       case may_rpl_msg of
         Nothing       -> do waitForResponseTo cid mid chan
         Just repl_msg -> if Telegram.message_id repl_msg == mid
                            then return msg
                            else waitForResponseTo cid mid chan

waitForResponseToOneOf :: [Telegram.Message] -> TChan Telegram.Update -> IO Telegram.Message
waitForResponseToOneOf msgs chan
  = do update <- atomically $ readTChan chan
       case Telegram.reply_to_message $ Telegram.message $ update of
         Nothing       -> waitForResponseToOneOf msgs chan
         Just repl_msg -> if repl_msg `elem` msgs
                            then return $ Telegram.message update
                            else waitForResponseToOneOf msgs chan

getContentFromWaitResponse :: Int -> Int -> TChan Telegram.Update -> IO String
getContentFromWaitResponse cid mid chan
  = do msg <- waitForResponseTo cid mid chan
       return $ Telegram.text msg

readFn :: Telegram.Message -> Telegram.Message -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO String
readFn edit_msg prog_msg chan last_msgs
  = do maybe_msg <- atomically $ readTVar last_msgs
       msg <- case maybe_msg of
                []       -> do inp_msg <- Send.editSimpleMessage edit_msg "(input required)"
                               atomically $ modifyTVar last_msgs (\lst -> inp_msg:lst)
                               waitForResponseToOneOf [inp_msg] chan

                inp_msgs -> waitForResponseToOneOf inp_msgs chan
       
       atomically $ modifyTVar last_msgs (\lst -> msg:lst)
       let content = Telegram.text msg
       when (content == "/kill") $ fail "Program terminated by user"
       return content

writeFn :: Telegram.Message -> Telegram.Message -> TChan Telegram.Update -> TVar [Telegram.Message] -> String -> IO ()
writeFn edit_msg prog_msg chan last_msgs out
  = do mid <- atomically $ getMayMid
       let cid = Telegram.getMessageChatID prog_msg
       let prog_mid = Telegram.getMessageID prog_msg
       msg <- Send.sendMessage $ Send.replyMessage cid out $ mid ?? prog_mid
       atomically $ modifyTVar last_msgs (\lst -> msg:lst)
  where getMayMid :: STM (Maybe Int)
        getMayMid = do val <- readTVar last_msgs
                       return $ Telegram.message_id <$> listToMaybe val

customSystem :: Telegram.Message -> Telegram.Message -> TChan Telegram.Update -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO System
customSystem edit_msg prog_msg ichan ochan last_msgs
  = do ds <- defaultSystem
       return ds { read_fn  = readFn  edit_msg prog_msg ochan last_msgs
                 , write_fn = writeFn edit_msg prog_msg ichan last_msgs
                 , time_limit = 60
                 }

customEnvironment :: Telegram.Message -> Telegram.Message -> TChan Telegram.Update -> TVar [Telegram.Message] -> IO Env
customEnvironment edit_msg prog_msg chan last_msgs
  = do ichan <- atomically $ dupTChan chan
       ochan <- atomically $ dupTChan chan
       sys   <- customSystem edit_msg prog_msg ichan ochan last_msgs
       env   <- defaultEnv
       return env { system = sys }

brainfuckCommand :: EntrypointM ()
brainfuckCommand = command "/brainfuck" $ do
  update  <- getCallerUpdate
  chan    <- getUpdateChannel
  program <- getContentAfterCommand
  msg <- getCallerMessage
  case parseInstructions program of
    Left err   -> do liftIO $ Send.sendSimpleReplyTo msg err ; return Nothing
    Right []   -> do liftIO $ Send.sendSimpleReplyTo msg "No instructions in program." ; return Nothing
    Right inst -> do replMsg <- liftIO $ Send.sendSimpleReplyTo msg "Executing your code..."
                     liftIO $ forkIO $ runBF replMsg chan msg inst
                     return Nothing

  where runBF :: Telegram.Message -> TChan Telegram.Update -> Telegram.Message -> [Instruction] -> IO ()
        runBF edit_msg chan msg inst = do
          last_msgs <- atomically $ newTVar []
          env       <- customEnvironment edit_msg msg chan last_msgs
          res       <- execute (interpret inst) env
          case res of
            Left err     -> Send.editSimpleMessage edit_msg $ "Error(s):\n" <> err
            Right (_, _) -> Send.editSimpleMessage edit_msg "Finished execution"
          
          return ()
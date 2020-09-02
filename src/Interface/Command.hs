module Interface.Command
  ( BroadChan
  , Config
  , CommandM
  , CommandActionT
  , CommandActionM
  , sequenceCommands
  , act
  , command
  , getSeqCommand
  , getSeqChannel
  , getSeqConfig
  , dupSeqChannel
  , getCallerCommand
  , getCommandChannel
  , getConfig
  , getContentAfterCommand
  , getNextCommand
  , replyText
  )
  where

import           Data.List
import           GHC.Base (ap)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TChan as STM

import qualified Dataproviders.Logger         as Logger
import qualified Entity.Question              as Q
import qualified Entity.Message               as Msg
import qualified Entity.Answer                as A
import qualified Entity.Command               as Cmd
import qualified Interface.SendMessage        as I


type BroadChan = STM.TChan Cmd.Command
type Config c = (c, Cmd.Command, BroadChan)
type CommandM c = CommandT (Config c) IO
type CommandActionM c = CommandActionT (Config c) IO

-- Sequence of all commands.
newtype CommandT c m a
  = CommandT { sequenceCommands :: c -> m a }

instance Monad m => Functor (CommandT c m) where
  fmap f x = x >>= (pure . f)

instance Monad m => Applicative (CommandT c m) where
  pure x = CommandT $ \_ -> return x
  (<*>) = ap

instance Monad m => Monad (CommandT c m) where
  (>>=) x f = CommandT $
    \c -> do a <- {- NOTE: error handling here... -}sequenceCommands x c
             sequenceCommands (f a) c

instance MonadIO m => MonadIO (CommandT c m) where
  liftIO action = CommandT $ \_ -> liftIO $ action

instance MonadTrans (CommandT c) where
  lift x = CommandT $ \_ -> x

getSeqCommand :: Monad m => CommandT (Config c) m Cmd.Command
getSeqCommand = CommandT $
  \(_, cmd, _) -> return cmd

getSeqChannel :: Monad m => CommandT (Config c) m BroadChan
getSeqChannel = CommandT $
  \(_, _, chan) -> return chan

getSeqConfig :: Monad m => CommandT (Config c) m c
getSeqConfig = CommandT $
  \(cfg, _, _) -> return cfg

dupSeqChannel :: MonadIO m => CommandT (Config c) m (STM.TChan Cmd.Command)
dupSeqChannel = do
  chan <- getSeqChannel
  liftIO $ STM.atomically $ STM.dupTChan chan

-- Things that a command may do.
data CommandActionT c m a
  = CommandActionT { act :: c -> m (Either String a)}

instance Monad m => Functor (CommandActionT c m) where
  fmap f x = x >>= (pure . f)

instance Monad m => Applicative (CommandActionT c m) where
  pure x = CommandActionT $ \_-> return $ Right x
  (<*>) = ap

instance Monad m => Monad (CommandActionT c m) where
  (>>=) x f = CommandActionT $
    \context -> do res <- act x context
                   case res of
                     Left err -> return $ Left err
                     Right a  -> act (f a) context

instance MonadIO m => MonadIO (CommandActionT c m) where
  liftIO action = CommandActionT $
    \_ -> do res <- liftIO $ action
             return $ Right res

instance Monad m => MonadFail (CommandActionT c m) where
  fail str = CommandActionT $ \_ -> return $ Left str

instance MonadTrans (CommandActionT c) where
  lift x = CommandActionT $ \_ -> Right <$> x

getCallerCommand :: Monad m => CommandActionT (Config c) m Cmd.Command
getCallerCommand = CommandActionT $
  \(_, cmd, _) -> return $ Right cmd

getCommandChannel :: Monad m => CommandActionT (Config c) m (STM.TChan Cmd.Command)
getCommandChannel = CommandActionT $ \(_, _, chan) -> return $ Right chan

getConfig :: Monad m => CommandActionT (Config c) m c
getConfig = CommandActionT $ \(cfg, _, _) -> return $ Right cfg

getContentAfterCommand :: Monad m => CommandActionT (Config c) m String
getContentAfterCommand = do
  cmd <- getCallerCommand
  return $ Cmd.getContent cmd

getNextCommand :: MonadIO m => CommandActionT (Config c) m Cmd.Command
getNextCommand = do
  chan <- getCommandChannel
  liftIO $ STM.atomically $ STM.peekTChan chan

replyText :: (I.SendMessage m, Monad m) => String -> CommandActionT (Config c) m Cmd.MessageId
replyText str = do
  cmd <- getCallerCommand
  lift $ I.sendMessage $ Msg.replyToCommand cmd str

sendText :: (I.SendMessage m, Monad m) => String -> CommandActionT (Config c) m Cmd.MessageId
sendText str = do
  cmd <- getCallerCommand
  lift $ I.sendMessage $ Msg.simple (Cmd.getChatId cmd) str

-- Creates a bot command listener
command :: (I.SendMessage m, MonadIO m) => String -> CommandActionT (Config c) m (Maybe Msg.Message) -> CommandT (Config c) m ()
command cmdname action = do
  cfg  <- getSeqConfig
  cmd  <- getSeqCommand
  chan <- getSeqChannel
  case Cmd.getCommand cmd of
    Nothing   -> return ()
    Just comm ->
      when (cmdname `isPrefixOf` comm) $ do
        chan' <- dupSeqChannel
        res   <- lift $ act action (cfg, cmd, chan')
        case res of
          Left err   -> do liftIO $ Logger.log $ "An error occurred while performing an action: " <> err
                           lift $ I.sendMessage $ Msg.replyToCommand cmd "Erro ao interpretar comando."
                           return ()
          Right mMsg -> sequence_ $ (lift . I.sendMessage) <$> mMsg
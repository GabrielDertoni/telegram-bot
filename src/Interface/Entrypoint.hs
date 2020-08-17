module Interface.Entrypoint where

import           GHC.Base (ap)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TChan as STM

import qualified Entity.Question              as Q
import qualified Entity.Message               as M
import qualified Entity.Answer                as A
import qualified Helper.Telegram.Types        as Telegram
import qualified Helper.Telegram              as Telegram
import qualified Controller.SendMessage       as Send
import qualified Helper.Telegram.SendMessage  as Method


type BroadChan = STM.TChan Telegram.Update

newtype EntrypointM a
  = EntrypointM { sequenceEntrypoints :: (Telegram.Update, BroadChan) -> IO a }

instance Functor EntrypointM where
  fmap f x = x >>= (pure . f)

instance Applicative EntrypointM where
  pure x = EntrypointM $ \(f, s) -> return x
  (<*>) = ap

instance Monad EntrypointM where
  (>>=) x f = EntrypointM $
    \(up, chan) -> do a <- {- NOTE: IO error handling here... -}sequenceEntrypoints x (up, chan)
                      sequenceEntrypoints (f a) (up, chan)

instance MonadIO EntrypointM where
  liftIO action = EntrypointM $ \_ -> action

data EntrypointActionM a
  = EntrypointActionM { act :: (String, Telegram.Update, BroadChan) -> IO (Either String a)}

instance Functor EntrypointActionM where
  fmap f x = x >>= (pure . f)

instance Applicative EntrypointActionM where
  pure x = EntrypointActionM $ \_-> return $ Right x
  (<*>) = ap

instance Monad EntrypointActionM where
  (>>=) x f = EntrypointActionM $
    \context -> do res <- act x context
                   case res of
                     Left err -> return $ Left err
                     Right a  -> act (f a) context

instance MonadIO EntrypointActionM where
  liftIO action = EntrypointActionM $
    \_ -> do res <- action
             return $ Right res

-- Creates a bot command listener
command :: String -> EntrypointActionM (Maybe M.Message) -> EntrypointM ()
command cmd action = EntrypointM $
  \(up, chan) -> do
    let msg = Telegram.message up
    case Telegram.getCommand msg of
      Nothing -> return ()
      Just (command, content) ->
        when (cmd == command) $ do
          chan' <- STM.atomically $ STM.dupTChan chan
          res   <- act action (content, up, chan')
          case res of
            Left err   -> do putStrLn $ "An error occurred while performing an action: " <> err
                             Send.sendReplyTo msg $ M.simpleMessage "Erro ao interpretar comando."
                             return ()
            Right mMsg -> sequence_ $ (Send.sendReplyTo msg) <$> mMsg

getCallerUpdate :: EntrypointActionM Telegram.Update
getCallerUpdate = EntrypointActionM $ \(_, up, _) -> return $ Right up

getCallerMessage :: EntrypointActionM Telegram.Message
getCallerMessage = do
  update <- getCallerUpdate
  return $ Telegram.message update

getCallerCommand :: EntrypointActionM String
getCallerCommand = do
  message <- getCallerMessage
  return $ maybe "" fst $ Telegram.getCommand message

getContentAfterCommand :: EntrypointActionM String
getContentAfterCommand = EntrypointActionM $ \(content, _, _) -> return $ Right content

getUpdateChannel :: EntrypointActionM (STM.TChan Telegram.Update)
getUpdateChannel = EntrypointActionM $ \(_, _, chan) -> return $ Right chan

getNextUpdate :: EntrypointActionM Telegram.Update
getNextUpdate = EntrypointActionM $
  \(_, _, chan) -> do res <- STM.atomically $ STM.peekTChan chan
                      return $ Right res
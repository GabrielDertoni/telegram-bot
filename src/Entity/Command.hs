module Entity.Command
  ( MessageId
  , Command(..)
  , getId
  , getChatId
  , getContent
  , getCommand
  , getReplyingTo
  , simple
  , reply
  , mension
  )
  where

-- Commands are the way users interact with the bot. Input may only be received through commands.

type MessageId = (Int, Int)

data Command
  -- Commands of the sort /command
  = Command { command :: String
            , content :: String
            , command_id :: MessageId
            }
  -- Replies to messages sent by the bot
  | Reply   { content :: String
            , replying_to :: MessageId
            , command_id :: MessageId
            }
  -- Mensions of the bot
  | Mension { content :: String
            , command_id :: MessageId
            }
  deriving (Show)

instance Eq Command where
  a == b = command_id a == command_id b

getId :: Command -> MessageId
getId = command_id

getChatId :: Command -> Int
getChatId = fst . command_id

getContent :: Command -> String
getContent = content

getCommand :: Command -> Maybe String
getCommand Command { command = str } = Just str
getCommand _ = Nothing

getReplyingTo :: Command -> Maybe MessageId
getReplyingTo Reply { replying_to = repl } = Just repl
getReplyingTo _ = Nothing

simple :: MessageId -> String -> String -> Command
simple cid cmd cont
  = Command { command    = cmd
            , content    = cont
            , command_id = cid
            }

reply :: MessageId -> String -> MessageId -> Command
reply cid cont replid
  = Reply { content     = cont
          , replying_to = replid
          , command_id  = cid
          }

mension :: MessageId -> String -> Command
mension cid cont
  = Mension { content    = cont
            , command_id = cid
            }

{-
createCommand :: Maybe String -> String -> Int -> Maybe Int -> Command
createCommand (Just cmd) cont cid _
  = Command { command    = cmd
            , content    = cont
            , command_id = cid
            }
createCommand Nothing cont cid (Just replid)
  = Reply { content     = cont
          , replying_to = replid
          , command_id  = cid
          }
createcommand Nothing cont cid Nothing
  = Mension { content    = cont
            , command_id = cid
            }
-}
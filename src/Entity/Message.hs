module Entity.Message
  ( Message(..)
  , getMessageText
  , maybeGetMessageText
  , getMessageImage
  , getMessageReplyId
  , getMessageChatId
  , simple
  , image
  , reply
  , replyToCommand
  , editText
  , noContent
  , hello
  )
  where

import qualified Entity.Command as Cmd

data Message
  = TextMessage     { text :: String
                    , chat_id :: Int
                    , reply_message_id :: Maybe Int
                    }
  | ImageMessage    { text :: String
                    , chat_id :: Int
                    , imageURL :: String
                    , reply_message_id :: Maybe Int
                    }
  | EditMessageText { text :: String
                    , chat_id :: Int
                    , edit_message_id :: Int
                    }
  deriving (Eq, Show)

getMessageText :: Message -> String
getMessageText = text

maybeGetMessageText :: Message -> Maybe String
maybeGetMessageText msg = case filter (/= ' ') $ getMessageText msg of
                            []  -> Nothing
                            str -> Just str

getMessageImage :: Message -> Maybe String
getMessageImage ImageMessage { imageURL = url } = Just url
getMessageImage _ = Nothing

getMessageReplyId :: Message -> Maybe Int
getMessageReplyId TextMessage { reply_message_id = mid } = mid
getMessageReplyId ImageMessage { reply_message_id = mid } = mid
getMessageReplyId _ = Nothing

getMessageChatId :: Message -> Int
getMessageChatId = chat_id

simple :: Int -> String -> Message
simple cid text
  = TextMessage { text = text
                , chat_id = cid
                , reply_message_id = Nothing
                }

image :: Int -> String -> Message
image cid imgURL
  = ImageMessage { imageURL = imgURL
                 , text = ""
                 , chat_id = cid
                 , reply_message_id = Nothing
                 }

reply :: (Int, Int) -> String -> Message
reply (cid, mid) text = (simple cid text) { reply_message_id = Just mid }

noContent :: Int -> Message
noContent = flip simple "É necessário algum conteúdo após o comando."

hello :: Int -> Message
hello = flip simple "Olá você(s)"

replyToCommand :: Cmd.Command -> String -> Message
replyToCommand = reply . Cmd.getId

editText :: (Int, Int) -> String -> Message
editText (cid, editId) txt
  = EditMessageText { text            = txt
                    , chat_id         = cid
                    , edit_message_id = editId
                    }

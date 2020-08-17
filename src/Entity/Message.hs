module Entity.Message where

data Message
  = TextMessage  { text :: String
                --  , chat_id :: Int
                --  , message_id :: Int
                 }
  | ImageMessage { text :: String
                --  , chat_id :: Int
                --  , message_id :: Int
                 , image :: String
                 }
  deriving (Eq, Show)

simpleMessage :: String -> Message
simpleMessage text = TextMessage { text = text }

imageMessage :: String -> Message
imageMessage imgURL = ImageMessage { image = imgURL, text = "" }

noContentMessage :: Message
noContentMessage = simpleMessage "É necessário algum conteúdo após o comando."

helloMessage :: Message
helloMessage = simpleMessage "Olá você(s)"
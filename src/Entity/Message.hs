module Entity.Message where

data Message
  = TextMessage  { text :: String
                 }
  | ImageMessage { image :: String
                 , caption :: String
                 }
  deriving (Eq, Show)

simpleMessage :: String -> Message
simpleMessage text = TextMessage { text = text }

imageMessage :: String -> Message
imageMessage imgURL = ImageMessage { image = imgURL, caption = "" }

noContentMessage :: Message
noContentMessage = simpleMessage "É necessário algum conteúdo após o comando."

helloMessage :: Message
helloMessage = simpleMessage "Olá você(s)"
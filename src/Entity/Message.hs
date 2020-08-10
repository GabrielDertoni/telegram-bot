module Entity.Message where

data Message
  = TextMessage  { text :: String
                 }
  | ImageMessage { image :: String
                 , caption :: String
                 }
  deriving (Eq, Show)
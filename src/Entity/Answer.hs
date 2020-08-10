module Entity.Answer where

data Answer
  = Answer { text :: String
           , image :: Maybe String
           } deriving (Eq, Show)
module Entity.Question where

data Question
  = Question { text :: String
             } deriving (Eq, Show)
module Helper.Query where

class Query a where
  getURL :: a -> String
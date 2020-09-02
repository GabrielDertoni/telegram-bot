module Interface.GetIDK
  ( GetIDK
  , getIDK
  )
  where

class GetIDK a where
  getIDK :: a -> IO String
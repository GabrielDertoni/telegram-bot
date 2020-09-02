module Interface.GetFunfact
  ( GetFunfact
  , getFunfact
  )
  where


class GetFunfact a where
  getFunfact :: a -> IO String
module Dataproviders.FunfactDataprovider
  ( FunfactDataprovider
  , funfactDataprovider
  )
  where

import qualified Interface.GetFunfact              as I
import qualified Interface.BotInfo                 as I

data FunfactDataprovider a
  = FunfactDataprovider { fname :: String
                        , botInfo :: a
                        }

funfactDataprovider :: a -> FunfactDataprovider a
funfactDataprovider binf
  = FunfactDataprovider { fname   = "./assets/funfacts.txt"
                        , botInfo = binf
                        }

instance I.BotInfo a => I.GetFunfact (FunfactDataprovider a) where
  getFunfact provider = do
    lst <- lines <$> readFile (fname provider)
    info <- I.getIncFunfactInfo $ botInfo provider
    return $ lst !! fromIntegral (info `mod` fromIntegral (length lst))


module Dataproviders.IDKDataprovider
  ( IDKDataprovider
  , idkDataprovider
  )
  where

import           System.Random

import           Helper.File
import qualified Interface.GetIDK as I

data IDKDataprovider
  = IDKDataprovider { fname :: String
                    }

idkDataprovider :: IDKDataprovider
idkDataprovider = IDKDataprovider { fname = "./assets/idks.txt" }

instance I.GetIDK IDKDataprovider where
  getIDK provider = do
    lst <- lines <$> readFileAsUTF8 (fname provider)
    drop 3 <$> choice lst

choice :: [a] -> IO a
choice lst = (lst !!) <$> randomRIO (0, length lst - 1)
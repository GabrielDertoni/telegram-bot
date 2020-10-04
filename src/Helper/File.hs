module Helper.File
  ( readFileAsUTF8
  , writeFileAsUTF8
  ) where

import           GHC.IO.Handle
import           System.IO
import qualified System.IO.Strict as SIO

readFileAsUTF8 :: FilePath -> IO String
readFileAsUTF8 path = 
  withFile path ReadMode $ \handle -> do
    hLock handle ExclusiveLock
    hSetEncoding handle utf8
    contents <- SIO.hGetContents handle
    return contents

writeFileAsUTF8 :: FilePath -> String -> IO ()
writeFileAsUTF8 path contents =
  withFile path WriteMode $ \handle -> do
    hLock handle ExclusiveLock
    hSetEncoding handle utf8
    hPutStr handle contents
module Helper.File
  ( readFileAsUTF8
  , writeFileAsUTF8
  ) where

import           System.IO

readFileAsUTF8 :: FilePath -> IO String
readFileAsUTF8 path = do
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  hGetContents handle

writeFileAsUTF8 :: FilePath -> String -> IO ()
writeFileAsUTF8 path contents = do
  handle <- openFile path WriteMode
  hSetEncoding handle utf8
  hPutStr handle contents
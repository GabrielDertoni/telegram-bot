module Dataproviders.Logger
  ( Dataproviders.Logger.log
  , getLogs
  )
  where

import Text.Printf
import Data.Time
import Control.Exception (SomeException, handle)

import Configuration.AppConfig

log :: String -> IO ()
log str = do
  ctxt <- getContext
  time <- getCurrentTime
  case ctxt of
    Production  -> appendFile logsFilePath $ printf"[%s] %s\n" (show time) str
    Development -> putStrLn str

getLogs :: IO [String]
getLogs = do
  ctxt <- getContext
  case ctxt of
    Development -> return []
    Production  -> handle returnEmptyListOnError $
                    do content <- readFile logsFilePath
                       return $ lines content

returnEmptyListOnError :: SomeException -> IO [String]
returnEmptyListOnError _ = pure []

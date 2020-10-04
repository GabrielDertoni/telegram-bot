module Helper.Request
  ( requestHttp
  , curlHttp
  ) where

import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import Data.ByteString.Lazy.UTF8 (fromString, ByteString)
import System.Process

curlHttp :: String -> IO ByteString
curlHttp url = fromString <$> readProcess "curl" ["-s", url] []

requestHttp :: String -> IO ByteString
requestHttp url = do
  req <- parseRequest url
  resp <- httpLBS req
  return $ getResponseBody resp
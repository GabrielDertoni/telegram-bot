module Dataproviders.AskDataprovider
  ( askDataproviderImplementation
  , constructQuery
  ) where

import qualified Network.URI.Encode as URI
import qualified Data.Aeson         as Aeson
import Network.HTTP.Conduit

import qualified Entity.Question            as Q
import qualified Entity.Answer              as A
import qualified Secret.WolframAPI          as API
import qualified Helper.Wolfram.APIResponse as Wolfram
import           Helper.Maybe
import           Interface.GetAnswer

data AskWolfram
  = AskWolfram { appId :: String
               , format :: String
               , podstate :: String
               , includepodid :: String
               }

instance GetAnswer AskWolfram where
  getAnswer dataprov (Q.Question str)
    = let urlQuery = constructQuery dataprov str in
      do response <- simpleHttp urlQuery
         case Aeson.eitherDecode response of
           Right result -> processResponse result
           Left err     -> fail err

baseURL :: String
baseURL = "https://api.wolframalpha.com/v2/query?output=json"

askDataproviderImplementation :: AskWolfram
askDataproviderImplementation
  = AskWolfram { appId = API.appId
               , format = "plaintext,image"
               , podstate = "Result__Step-by-step+solution"
               , includepodid = "Result"
               }

processResponse :: Wolfram.APIResponse -> IO A.Answer
processResponse response
  = maybe (fail $ "Couldn't find the necessary data in " <> (show response)) return $
    do text <- Wolfram.plaintext $ head subpods
       img  <- Wolfram.img (subpods !! 1)
       return A.Answer { A.text = text
                       , A.image = Wolfram.src img
                       }
    where subpods = Wolfram.subpods pod
          pod     = head $ Wolfram.pods qresult
          qresult = Wolfram.queryresult response


constructQuery :: AskWolfram -> String -> String
constructQuery wolfram input = baseURL
                            <> "&appid="
                            <> (appId wolfram)
                            <> "&format="
                            <> (format wolfram)
                            <> "&podstate="
                            <> (podstate wolfram)
                            <> "&includepodid="
                            <> (includepodid wolfram)
                            <> "&input="
                            <> (URI.encode input)
module Dataproviders.AskDataprovider
  ( AskWolfram(..)
  , askDataproviderImplementation
  ) where

import qualified Network.URI.Encode          as URI
import qualified Data.Aeson                  as Aeson
import           Network.HTTP.Conduit
import           Data.Maybe
import           Control.Exception

import qualified Entity.Question             as Q
import qualified Entity.Answer               as A
import qualified Configuration.WolframConfig as API
import qualified Helper.Wolfram.APIResponse  as Wolfram
import qualified Helper.Wolfram              as Wolfram
import           Helper.Maybe
import           Helper.Query
import           Interface.GetAnswer

data AskWolfram
  = AskWolfram { format :: String
               , podstate :: String
               , includepodid :: [String]
               }

instance GetAnswer AskWolfram where
  getAnswer dataprov (Q.Question str)
    = flip catch handleResponseException $
        do url <- getURL dataprov
           let urlQuery = url <:> ("input" <=> URI.encode str)
           putStrLn ("Querying: " ++ urlQuery)
           response <- simpleHttp urlQuery
           case Aeson.eitherDecode response of
             Right result -> processResponse result
             Left err     -> fail err

instance Query AskWolfram where
  getURL wolfram = do
    appId <- API.getAppId
    return $
      Wolfram.baseURL
      <:> "output=json"
      <:> ("appid" <=> appId)
      <:> ("format" <=> format wolfram)
      <:> ("podstate" <=> podstate wolfram)
       <> (foldl (<:>) "" $ map ("includepodid" <=>) $ includepodid wolfram)


askDataproviderImplementation :: AskWolfram
askDataproviderImplementation
  = AskWolfram { format = "plaintext,image"
               , podstate = "Step-by-step%20solution"
               , includepodid = ["Result", "Input"]
               }

processResponse :: Wolfram.APIResponse -> IO A.Answer
processResponse response
  = maybe (fail $ "Couldn't find the necessary data in " <> (show response)) return $
    do pod <- listToMaybe $ Wolfram.pods qresult
       fstSubpod <- listToMaybe $ Wolfram.subpods pod
       let sndSubpod = last $ Wolfram.subpods pod
       text <- Wolfram.plaintext fstSubpod
       img  <- Wolfram.img sndSubpod
       return A.Answer { A.text = text
                       , A.image = Just $ Wolfram.src img
                       }
    where qresult = Wolfram.queryresult response

handleResponseException :: IOException -> IO A.Answer
handleResponseException exception
  = do print exception
       return A.Answer { A.text  = "Failed to fetch answer from the Wolfram API...\nMaybe try a different question."
                       , A.image = Nothing
                       }
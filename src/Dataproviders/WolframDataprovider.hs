module Dataproviders.WolframDataprovider
  ( WolframDataprovider(..)
  , wolframDataprovider
  ) where

import qualified Network.URI.Encode          as URI
import qualified Data.Aeson                  as Aeson
import           Data.Maybe
import           Control.Exception

import           Helper.Request
import qualified Dataproviders.Logger        as Logger
import qualified Entity.Question             as Q
import qualified Entity.Answer               as A
import qualified Configuration.WolframConfig as API
import qualified Helper.Wolfram.APIResponse  as Wolfram
import qualified Helper.Wolfram              as Wolfram
import           Helper.Maybe
import           Helper.Query
import           Interface.GetAnswer

data WolframDataprovider
  = WolframDataprovider { format :: String
                        , podstate :: String
                        , includepodid :: [String]
                        }

instance GetAnswer WolframDataprovider where
  getAnswer dataprov (Q.Question str)
    = flip catch handleResponseException $
        do url <- getURL dataprov
           let urlQuery = url <:> ("input" <=> URI.encode str)
           Logger.log ("Querying: " ++ urlQuery)
           response <- requestHttp urlQuery
           case Aeson.eitherDecode response of
             Right result -> processResponse result
             Left err     -> fail err

instance Query WolframDataprovider where
  getURL wolfram = do
    query <- wolframQuery wolfram
    return $ Wolfram.baseURL <> query

wolframQuery :: WolframDataprovider -> IO String
wolframQuery provider = do
    appId <- API.getAppId
    return $
      fromPairs [ ("output", "json")
                , ("appid", appId)
                , ("format", format provider)
                , ("podstate", podstate provider)
                ]

wolframDataprovider :: WolframDataprovider
wolframDataprovider
  = WolframDataprovider { format = "plaintext,image"
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
  = do Logger.log $ show exception
       return A.Answer { A.text  = "Failed to fetch answer from the Wolfram API...\nMaybe try a different question."
                       , A.image = Nothing
                       }

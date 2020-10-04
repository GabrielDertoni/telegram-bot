module Dataproviders.FunfactDataprovider
  ( FunfactDataprovider
  , funfactDataprovider
  )
  where

import           GHC.Generics (Generic)
import           Data.Aeson ((.:))
import qualified Data.Aeson                        as Aeson
import           System.Environment
import           Network.HTTP.Conduit (simpleHttp)

import           Helper.Request
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
    firestore_url <- getEnv "FIRESTORE_URL"
    info <- I.getIncFunfactInfo $ botInfo provider
    let url = firestore_url <> "databases/(default)/documents/funfacts/" <> show (info `mod` 100)
    resp <- requestHttp url
    case Aeson.decode resp of
      Nothing -> fail "Could't retrieve funfact."
      Just b  -> return $ funfact $ fields b

data FirestoreGetDocumentResponse a
  = FirestoreGetDocumentResponse { path :: String
                                 , fields :: a
                                 }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON a => Aeson.FromJSON (FirestoreGetDocumentResponse a) where
  parseJSON (Aeson.Object v) = do
    path   <- v .: "name"
    fields <- v .: "fields"
    return $ FirestoreGetDocumentResponse { path   = path
                                          , fields = fields
                                          }

data FunfactDocument
  = FunfactDocument { funfact :: String
                    }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON FunfactDocument where
  parseJSON (Aeson.Object v) = do
    dat     <- v .: "data"
    funfact <- dat .: "stringValue"
    return $ FunfactDocument { funfact = funfact }
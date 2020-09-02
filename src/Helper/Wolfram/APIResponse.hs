module Helper.Wolfram.APIResponse where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.:?))

import Helper.Maybe

data APIResponse
  = APIResponse { queryresult :: QueryResult
                }
  deriving (Eq, Show, Generic)

data QueryResult
  = QueryResult { success :: Bool
                , pods :: [Pod]
                }
  deriving (Eq, Show, Generic)

data Pod
  = ResultPod  { title :: String
               , subpods :: [Subpod]
               , primary :: Bool
               }
  | InputPod   { title :: String
               , subpods :: [Subpod]
               , primary :: Bool
               }
  | GenericPod { title :: String
               , pod_id :: String
               , subpods :: [Subpod]
               , primary :: Bool
               }
  deriving (Eq, Show, Generic)

data Subpod
  = Subpod { sub_title :: String
           , img :: Maybe Image
           , plaintext :: Maybe String
           }
  deriving (Eq, Show, Generic)

data Image
  = Image { src :: String
          , img_title :: String
          , width :: Int
          , height :: Int
          }
  deriving (Eq, Show, Generic)



-- JSON Parsing --

instance Aeson.FromJSON APIResponse where
  parseJSON (Aeson.Object v) = APIResponse <$> v .: "queryresult"

instance Aeson.FromJSON QueryResult where
  parseJSON (Aeson.Object v) = do
    success <- v .:  "success"
    pods    <- v .:? "pods"
    return QueryResult { success = success
                       , pods    = filter isCorrectPod $ pods ?? []
                       }

isCorrectPod :: Pod -> Bool
isCorrectPod = primary

instance Aeson.FromJSON Pod where
  parseJSON (Aeson.Object v) = do
    title   <- v .:  "title"
    pid     <- v .:  "id"
    subpods <- v .:? "subpods"
    prim    <- maybe False id <$> v .:? "primary"
    case pid of
      "Result" -> return $
                    ResultPod { title   = title
                              , primary = prim
                              , subpods = subpods ?? []
                              }
      "Input"  -> return $
                    InputPod { title    = title
                             , primary = prim
                             , subpods = subpods ?? []
                             }
      _        -> return $
                    GenericPod { title   = title
                               , primary = prim
                               , pod_id  = pid
                               , subpods = subpods ?? []
                               }

instance Aeson.FromJSON Subpod where
  parseJSON (Aeson.Object v) = do
    sub_title <- v .:  "title"
    img       <- v .:? "img"
    plaintext <- v .:? "plaintext"
    return $
      Subpod { sub_title = sub_title
             , img       = img
             , plaintext = plaintext
             }

instance Aeson.FromJSON Image where
  parseJSON (Aeson.Object v) = Image
                           <$> v .: "src"
                           <*> v .: "title"
                           <*> v .: "width"
                           <*> v .: "height"
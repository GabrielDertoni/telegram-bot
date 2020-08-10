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
  = ResultPod  { result_title :: String
               , pod_id :: String
               , nsubpods :: Int
               , subpods :: [Subpod]
               }
  | InputPod   { result_title :: String
                , pod_id :: String
               , nsubpods :: Int
                , subpods :: [Subpod]
                }
  | GenericPod { result_title :: String
               , pod_id :: String
               , nsubpods :: Int
               , subpods :: [Subpod]
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
isCorrectPod (ResultPod _ _ nsub _) = nsub >= 2
isCorrectPod (InputPod _ _ nsub _) = nsub >= 2
isCorrectPod _ = False

instance Aeson.FromJSON Pod where
  parseJSON (Aeson.Object v) = do
    title   <- v .:  "title"
    id      <- v .:  "id"
    subpods <- v .:? "subpods"
    nsub    <- v .:  "numsubpods" :: Aeson.Parser Int
    case id of
      "Result" -> return $
                    ResultPod { result_title = title
                              , pod_id       = id
                              , nsubpods     = nsub
                              , subpods      = subpods ?? []
                              }
      "Input"  -> return $
                    InputPod { result_title = title
                              , pod_id       = id
                              , nsubpods     = nsub
                              , subpods      = subpods ?? []
                              }
      _        -> return $
                    GenericPod { result_title = title
                                , pod_id       = id
                                , nsubpods     = nsub
                                , subpods      = subpods ?? []
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
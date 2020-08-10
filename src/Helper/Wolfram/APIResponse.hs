module Helper.Wolfram.APIResponse where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

data APIResponse
  = APIResponse { queryresult :: QueryResult
                } deriving (Eq, Show, Generic)

data QueryResult
  = QueryResult { success :: Bool
                , pods :: [Pod]
                } deriving (Eq, Show, Generic)

data Pod
  = ResultPod { result_title :: String
              , pod_id :: String
              , subpods :: [Subpod]
              } deriving (Eq, Show, Generic)

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
          } deriving (Eq, Show, Generic)



-- JSON Parsing --

instance Aeson.FromJSON APIResponse where
  parseJSON (Aeson.Object v) = APIResponse <$> v .: "queryresult"

instance Aeson.FromJSON QueryResult where
  parseJSON (Aeson.Object v) = QueryResult
                           <$> v .: "success"
                           <*> v .: "pods"

instance Aeson.FromJSON Pod where
  parseJSON (Aeson.Object v) = do
    title   <- v .: "title"
    id      <- v .: "id"
    subpods <- v .: "subpods"
    if id == "Result"
      then return ResultPod { result_title = title
                            , pod_id       = id
                            , subpods      = subpods
                            }
    else fail ("Unidendified pod" ++ id)

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
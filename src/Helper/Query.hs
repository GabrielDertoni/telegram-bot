module Helper.Query where

import Data.List

class Query a where
  getURL :: a -> IO String

(<:>) :: String -> String -> String
(<:>) a b = a ++ "&" ++ b

(<:?>) :: String -> Maybe String -> String
(<:?>) [] b = maybe "" id  b
(<:?>) a  b = maybe a (a <:>) b

(<=>) :: String -> String -> String
(<=>) a b = a <> "=" <> b

fromPairs :: [(String, String)] -> String
fromPairs = intercalate "&" . map (uncurry (<=>)) . filter ((/= 0) . length . snd)
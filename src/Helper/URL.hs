module Helper.URL where

(<:>) :: String -> String -> String
(<:>) a b = a ++ "&" ++ b

(<:?>) :: String -> Maybe String -> String
(<:?>) [] b = maybe "" id  b
(<:?>) a  b = maybe a (a <:>) b

(<=>) :: String -> String -> String
(<=>) a b = a <> "=" <> b
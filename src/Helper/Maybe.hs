module Helper.Maybe where

-- equivalent to (??) a b = maybe b id a
(??) :: Maybe a -> a -> a
(??) (Just a) b = a
(??) Nothing  b = b

-- Join query operator.
(<??>) :: String -> Maybe String -> String
(<??>) url (Just opt) = url <> opt
(<??>) url Nothing    = url
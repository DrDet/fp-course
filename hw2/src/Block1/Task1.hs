module Block1.Task1
  (
    stringSum
  ) where

import           Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum s = fmap sum ml
  where
    l = words s
    ml = traverse readMaybe l

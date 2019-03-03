module Block1.Task4
  (
    stringSum
  ) where

stringSum :: String -> Int
stringSum s = foldr ((+) . read) 0 (words s)

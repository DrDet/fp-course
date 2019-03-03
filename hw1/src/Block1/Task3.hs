module Block1.Task3
  (
    contains
  ) where

contains :: Eq a => a -> [[a]] -> [[a]]
contains e = filter (elem e)

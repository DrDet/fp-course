module Block1.Task1
  (
    order3
  ) where

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z)
  | x <= min y z = (x, min y z, max y z)
  | y <= min x z = (y, min x z, max x z)
  | otherwise    = (z, min x y, max x y)

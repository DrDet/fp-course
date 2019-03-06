module Task4
  (
    iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import           Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = fix (\f x -> x : f x)

fibonacci :: Integer -> Integer
fibonacci = fix impl
  where
    impl = \f n ->
      if n > 2
      then f (n - 1) + f (n - 2)
      else 1

factorial :: Integer -> Integer
factorial = fix impl
  where
    impl = \f n ->
      if n > 0
      then n * f (n - 1)
      else 1

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix impl
  where
    impl = \f mapF l -> case l of
      x : xs -> mapF x : f mapF xs
      []     -> []

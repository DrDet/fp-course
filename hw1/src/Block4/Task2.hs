module Block4.Task2
  (
    splitOn
  , joinWith
  ) where

import           Data.List.NonEmpty (NonEmpty (..), (<|))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x = foldr (acc x) ([] :| [])
  where
    acc :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    acc delim e res@(r :| rs)
      | e /= delim    = (e : r) :| rs
      | otherwise     = [] <| res

joinWith :: a -> NonEmpty [a] -> [a]
joinWith delim (x :| xs) = x ++ foldr (\e r -> (delim : e) ++ r) [] xs

module Block5.Task1
  (
    maybeConcat
  , eitherConcat
  ) where

import           Data.Either (lefts, rights)

maybeConcat :: [Maybe [a]] -> Maybe [a]
maybeConcat = mconcat

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat l = (mconcat ls, mconcat rs)
  where
    ls = lefts l
    rs = rights l

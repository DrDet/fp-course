module Block2.Task2
  (
    mergeSort
  , randomIntList
  ) where

import           Data.List     (findIndex)
import           Data.Maybe    (fromMaybe)
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

mergeSort :: Ord a => [a] -> [a]
mergeSort l
  | len < 2   = l
  | otherwise = merge (mergeSort left) (mergeSort right)
    where
      len = length l
      mid = len `quot` 2
      (left, right) = splitAt mid l
      merge :: Ord a => [a] -> [a] -> [a]
      merge [] ys     = ys
      merge (x : xs) ys = beforeX ++ x : merge afterX xs
        where
          pos' = findIndex (x <=) ys
          pos = fromMaybe len pos'
          (beforeX, afterX) = splitAt pos ys

module Block1.Task2
  (
    smartReplicate
  ) where

smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\x -> (++) (replicate x x)) []

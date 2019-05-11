module Task1Utils
( multiplyNaive
, getRandomMatrix
) where

import           System.Random (getStdGen, randoms)

multiplyNaive :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyNaive m1 m2 = traverse processRow m1
  where
    processRow :: [Int] -> Maybe [Int]
    processRow row = traverse (scalarProduct row . getColumn) indices
      where
        indices = [0..(length $ head m2) - 1]
        scalarProduct :: [Int] -> [Int] -> Maybe Int
        scalarProduct l1 l2 =
          if (length l1) /= (length l2)
          then Nothing
          else Just $ foldr (\e res -> (fst e) * (snd e) + res) 0 pairs
            where
              pairs = zip l1 l2
    getColumn :: Int -> [Int]
    getColumn idx = map (!! idx) m2

getRandomMatrix :: Int -> Int -> IO [[Int]]
getRandomMatrix rows columns
  | rows == 0 = return []
  | otherwise = do
      g <- getStdGen
      let randomList = take columns (randoms g)
      rest <- (getRandomMatrix (rows - 1) columns)
      return (randomList : rest)

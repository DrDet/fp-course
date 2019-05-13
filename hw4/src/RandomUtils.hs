module RandomUtils
( getRandomMatrix
, getRandomPolygon
) where

import           System.Random (getStdGen, randomIO, randoms)
import           Task2         (Point (..))

getRandomMatrix :: Int -> Int -> IO [[Int]]
getRandomMatrix rows columns
  | rows == 0 = return []
  | otherwise = do
      g <- getStdGen
      let randomList = take columns (randoms g)
      rest <- (getRandomMatrix (rows - 1) columns)
      return (randomList : rest)

getRandomPolygon :: Int -> IO [Point]
getRandomPolygon n = do
  start <- randomIO :: IO Int
  step  <- randomIO :: IO Int
  return $ impl n start step
  where
    impl :: Int -> Int -> Int -> [Point]
    impl k start step
      | k == 0    = []
      | otherwise = p:(impl (k - 1) (start + step + 1) (step + 1))
          where
            p = Point start (start + step)

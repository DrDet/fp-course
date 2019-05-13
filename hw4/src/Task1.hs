module Task1
  ( multiplyNaive
  , multiply
  ) where

import           Control.Parallel.Strategies (Eval, parTraversable, rpar,
                                              runEval)
import qualified Data.Vector                 as V (Vector, foldr, fromList,
                                                   generate, head, length, map,
                                                   toList, zip, (!))

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

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply m1' m2' = fromMatrix <$> multiplyImpl m1'' m2''
  where
    toMatrix ll  = V.fromList (map V.fromList ll) :: V.Vector (V.Vector Int)
    fromMatrix m = V.toList (V.map V.toList m) :: [[Int]]
    m1''         = toMatrix m1'
    m2''         = toMatrix m2'
    multiplyImpl
      :: V.Vector (V.Vector Int)
      -> V.Vector (V.Vector Int)
      -> Maybe (V.Vector (V.Vector Int))
    multiplyImpl m1 m2 =
      if (V.length $ V.head m1) /= V.length m2
      then Nothing
      else Just $ runEval $ parTraversable processRow m1
        where
          processRow :: V.Vector Int -> Eval (V.Vector Int)
          processRow row = parTraversable
            (scalarProduct row . getColumn) indices
            where
              indices = V.generate (V.length $ V.head m2) id
              scalarProduct :: V.Vector Int -> V.Vector Int -> Eval Int
              scalarProduct v1 v2 = rpar $
                V.foldr (\e res -> (fst e) * (snd e) + res) 0 pairs
                  where
                    pairs = V.zip v1 v2
          getColumn :: Int -> V.Vector Int
          getColumn idx = V.map (V.! idx) m2

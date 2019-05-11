module Task1
  ( multiply
  ) where

import           Control.Parallel.Strategies (Eval, parTraversable, rseq,
                                              runEval)
import qualified Data.Vector                 as V (Vector, foldr, fromList,
                                                   generate, head, length, map,
                                                   toList, zip, (!))

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
              scalarProduct v1 v2 = rseq $
                V.foldr (\e res -> (fst e) * (snd e) + res) 0 pairs
                  where
                    pairs = V.zip v1 v2
          getColumn :: Int -> V.Vector Int
          getColumn idx = V.map (V.! idx) m2

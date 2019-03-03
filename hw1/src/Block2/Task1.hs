module Block2.Task1
  (
    removeAt
  ) where

removeAt :: Eq a => Int -> [a] -> Maybe (a, [a])
removeAt idx l = case rest of
  x:xs -> Just (x, start ++ xs)
  []   -> Nothing
  where
    (start, rest) = splitAt idx l

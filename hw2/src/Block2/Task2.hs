module Block2.Task2
  (
    moving
  ) where

import           Control.Monad.State (State, evalState, get, modify, return)
import           Data.Sequence       (Seq (..))

moving :: Int -> [Float] -> [Float]
moving n l = evalState (movingImpl l Empty []) 0
  where
    movingImpl :: [Float] -> Seq Float -> [Float] -> State Float [Float]
    movingImpl [] _ ans = return $ reverse ans
    movingImpl (x : xs) w ans
      | length w < n = do
          modify (+ x)
          cur <- get
          let w' = (w :|> x)
          movingImpl xs w' (cur / (fromIntegral $ length w') : ans)
      | (y :<| ys) <- w = do
          modify (+ (x - y))
          cur <- get
          let w' = (ys :|> x)
          movingImpl xs w' (cur / (fromIntegral $ length w') : ans)
      | otherwise = error "Window size must be > 0"

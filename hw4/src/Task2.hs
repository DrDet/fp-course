{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Task2
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  , perimeterNaive
  , doubleAreaNaive
  ) where

import           Control.Parallel.Strategies (NFData)
import           GHC.Generics                (Generic)

data Point = Point Int Int
  deriving (Show, Generic, NFData)

pointOpImpl :: (Int -> Int -> Int) -> Point -> Point -> Point
pointOpImpl op (Point x1 y1) (Point x2 y2) = Point (x1 `op` x2) (y1 `op` y2)

plus :: Point -> Point -> Point
plus = pointOpImpl (+)

minus :: Point -> Point -> Point
minus = pointOpImpl (-)

vectorLen :: Point -> Double
vectorLen (Point x y) = (sqrt . fromIntegral) (x * x + y * y)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

getEdgesNaive :: [Point] -> [(Point, Point)]
getEdgesNaive vs = zip vs (take (length vs) l)
  where
    l = (tail . cycle) vs

getEdges :: [Point] -> [(Point, Point)]
getEdges vs = myZip vs (tail vs)
  where
    myZip :: [Point] -> [Point] -> [(Point, Point)]
    myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
    myZip (lastEl:_) [] = [(lastEl, head vs)]
    myZip [] _          = []

perimeterNaive :: [Point] -> Double
perimeterNaive vs = sum lengths
  where
    lengths = map (\e -> vectorLen (snd e `minus` fst e)) edges
    edges   = getEdgesNaive vs

perimeter :: [Point] -> Double
perimeter !vs = sum $! lengths
  where
    lengths = map ((\e -> vectorLen (snd e `minus` fst e)) $!) $! edges
    edges   = getEdges vs

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive vs = sum areas
  where
    areas = map calcArea edges
    calcArea (Point x1 y1, Point x2 y2) = (y1 + y2) * (x1 - x2)
    edges = getEdgesNaive vs

doubleArea :: [Point] -> Int
doubleArea !vs = sum $! areas
  where
    areas = map (calcArea $!) $! edges
    calcArea (Point x1 y1, Point x2 y2) = (y1 + y2) * (x1 - x2)
    edges = getEdges vs

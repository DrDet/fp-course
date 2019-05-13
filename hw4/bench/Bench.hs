import           Criterion.Main (bench, bgroup, defaultMain, env, nf)
import           RandomUtils    (getRandomMatrix, getRandomPolygon)
import           Task1          (multiply, multiplyNaive)
import           Task2          (Point (..), doubleArea, doubleAreaNaive,
                                 perimeter, perimeterNaive)

setupMatrixEnv :: IO ([[Int]], [[Int]], [[Int]], [[Int]], [[Int]])
setupMatrixEnv = do
  m50_50   <- getRandomMatrix 50 50
  m50_100  <- getRandomMatrix 50 100
  m100_50  <- getRandomMatrix 100 50
  m100_100 <- getRandomMatrix 100 100
  m200_200 <- getRandomMatrix 200 200
  return (m50_50, m50_100, m100_50, m100_100, m200_200)

setupGeometryEnv :: IO ([Point], [Point])
setupGeometryEnv = do
  small <- getRandomPolygon 100000
  big   <- getRandomPolygon 10000000
  return (small, big)

main :: IO ()
main = defaultMain
  [
  env setupMatrixEnv $ \ ~(m50_50, m50_100, m100_50, m100_100, _) ->
    bgroup "naive matrix product"
      [ bench "50,50,50"    $ nf (multiplyNaive m50_50) m50_50
      , bench "50,100,100"  $ nf (multiplyNaive m50_100) m100_50
      , bench "100,50,100"  $ nf (multiplyNaive m100_50) m50_100
      , bench "100,100,100" $ nf (multiplyNaive m100_100) m100_100
      ]
  ,
  env setupMatrixEnv $ \ ~(m50_50, m50_100, m100_50, m100_100, m200_200) ->
    bgroup "optimized matrix product"
      [ bench "50,50,50"    $ nf (multiply m50_50) m50_50
      , bench "50,100,100"  $ nf (multiply m50_100) m100_50
      , bench "100,50,100"  $ nf (multiply m100_50) m50_100
      , bench "100,100,100" $ nf (multiply m100_100) m100_100
      , bench "200,200,200" $ nf (multiply m200_200) m200_200
      ]
  ,
  env setupGeometryEnv $ \ ~(small, big) -> bgroup "naive geometry"
    [ bench "perimeter 10^5"  $ nf perimeterNaive small
    , bench "doubleArea 10^5" $ nf doubleAreaNaive small
    , bench "perimeter 10^7"  $ nf perimeterNaive big
    , bench "doubleArea 10^7" $ nf doubleAreaNaive big
    ]
  ,
  env setupGeometryEnv $ \ ~(small, big) -> bgroup "optimized geometry"
    [ bench "perimeter 10^5"  $ nf perimeter small
    , bench "doubleArea 10^5" $ nf doubleArea small
    , bench "perimeter 10^7"  $ nf perimeter big
    , bench "doubleArea 10^7" $ nf doubleArea big
    ]
  ]

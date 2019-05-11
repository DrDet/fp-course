import           Criterion.Main
import           Task1          (multiply)
import           Task1Utils     (getRandomMatrix, multiplyNaive)

main :: IO ()
main = defaultMain [
  bgroup "matrix naive product"
    [ bench "naive product 50,50,50" $ nfIO $ do
        m1 <- (getRandomMatrix 50 50)
        m2 <- (getRandomMatrix 50 50)
        return (multiplyNaive m1 m2)
    , bench "naive product 50,100,100" $ nfIO $ do
        m1 <- (getRandomMatrix 50 100)
        m2 <- (getRandomMatrix 100 100)
        return (multiplyNaive m1 m2)
    , bench "naive product 100,50,100" $ nfIO $ do
        m1 <- (getRandomMatrix 100 50)
        m2 <- (getRandomMatrix 50 100)
        return (multiplyNaive m1 m2)
    , bench "naive product 100,100,100" $ nfIO $ do
        m1 <- (getRandomMatrix 100 100)
        m2 <- (getRandomMatrix 100 100)
        return (multiplyNaive m1 m2)
    ]
    ,
  bgroup "matrix optimized product"
    [ bench "optimized product 50,50,50" $ nfIO $ do
      m1 <- (getRandomMatrix 50 50)
      m2 <- (getRandomMatrix 50 50)
      return (multiply m1 m2)
    , bench "optimized product 50,100,100" $ nfIO $ do
        m1 <- (getRandomMatrix 50 100)
        m2 <- (getRandomMatrix 100 100)
        return (multiply m1 m2)
    , bench "optimized product 100,50,100" $ nfIO $ do
        m1 <- (getRandomMatrix 100 50)
        m2 <- (getRandomMatrix 50 100)
        return (multiply m1 m2)
    , bench "optimized product 100,100,100" $ nfIO $ do
        m1 <- (getRandomMatrix 100 100)
        m2 <- (getRandomMatrix 100 100)
        return (multiply m1 m2)
    , bench "optimized product 200,200,200" $ nfIO $ do
      m1 <- (getRandomMatrix 200 200)
      m2 <- (getRandomMatrix 200 200)
      return (multiply m1 m2)
    ]
  ]

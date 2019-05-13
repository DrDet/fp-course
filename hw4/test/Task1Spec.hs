module Task1Spec
( spec
) where

import           Data.Maybe  (fromMaybe)
import           RandomUtils (getRandomMatrix)
import           Task1       (multiply, multiplyNaive)
import           Test.Hspec  (Expectation, Spec, describe, it, shouldBe)

testProduct :: Int -> Int -> Int -> Expectation
testProduct a b c = do
  m1 <- getRandomMatrix a b
  m2 <- getRandomMatrix b c
  let x = fromMaybe [[0]] (multiply m1 m2)
      y = fromMaybe [[1]] (multiplyNaive m1 m2) 
  x `shouldBe` y

spec :: Spec
spec = describe "matrix product tests" $ do
  it "Correctness" $ do
      testProduct 1 1 1
      testProduct 1 2 1
      testProduct 2 2 1
      testProduct 1 2 3
      testProduct 2 3 4
      testProduct 12 14 29
      testProduct 23 17 47
      testProduct 42 42 42
      testProduct 42 105 20
  it "Errors" $ do
    multiply [[1]] [[1, 1],[1,1]] `shouldBe` Nothing
    multiply [[1, 1],[1,1]] [[1, 1],[1,1],[1,1]] `shouldBe` Nothing
    multiply [[1,2],[4,5]] [[1,2,2],[3,4,4],[5,6,6]] `shouldBe` Nothing

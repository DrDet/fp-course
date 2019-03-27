module Block1.Task1Spec
  (
    spec
  ) where

import           Test.Hspec (Spec, describe, it, shouldBe)
import           Block1.Task1 (stringSum)

spec :: Spec
spec = describe "stringSum tests" $ do
  it "return Just" $ do
    stringSum "1 2 3 4 5"   `shouldBe` Just 15
    stringSum "  0   12 -2" `shouldBe` Just 10
    stringSum "" `shouldBe` Just 0
  it "return Nothing" $ do
    stringSum "1 2 3 4 5 a" `shouldBe` Nothing
    stringSum "1 2 --3 4 5" `shouldBe` Nothing
    stringSum "a" `shouldBe` Nothing

module Block3.Task2Spec
(
  spec
) where

import           Block3.Task1 (Parser (..))
import           Block3.Task2 (element, stream)
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "basic combinators tests" $ do
  it "Correctness" $ do
    runParser (element 'a') "aba" `shouldBe` Just ('a', "ba")
    runParser (stream "aba") "aba" `shouldBe` Just ("aba", "")
  it "Errors" $ do
    runParser (element 'a') "1aba" `shouldBe` Nothing
    runParser (stream "aba") "baba" `shouldBe` Nothing

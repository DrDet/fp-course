module Block3.Task3Spec
(
  spec
) where

import           Block3.Task1 (Parser (..))
import           Block3.Task3 (parserCBS, parserDigit)
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "basic combinators tests" $ do
  it "Correctness" $ do
    runParser parserCBS "" `shouldBe` Just ((), "")
    runParser parserCBS "()" `shouldBe` Just ((), "")
    runParser parserCBS "()()" `shouldBe` Just ((), "")
    runParser parserCBS "(()(()))" `shouldBe` Just ((), "")
    runParser parserDigit "0" `shouldBe` Just (0, "")
    runParser parserDigit "42" `shouldBe` Just (42, "")
    runParser parserDigit "-42" `shouldBe` Just (-42, "")
    runParser parserDigit "-12345" `shouldBe` Just (-12345, "")
    runParser parserDigit "+12345" `shouldBe` Just (12345, "")
  it "Errors" $ do
    runParser parserCBS "(()(())))" `shouldBe` Nothing
    runParser parserCBS "(((()(()))" `shouldBe` Nothing
    runParser parserCBS "())" `shouldBe` Nothing
    runParser parserCBS "(()" `shouldBe` Nothing
    runParser parserDigit "0 0" `shouldBe` Nothing
    runParser parserDigit "0.0" `shouldBe` Nothing
    runParser parserDigit "Hello" `shouldBe` Nothing
    runParser parserDigit "--123" `shouldBe` Nothing
    runParser parserDigit "++123" `shouldBe` Nothing
    runParser parserDigit "a123" `shouldBe` Nothing

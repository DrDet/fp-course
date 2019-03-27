module Block2.Task1Spec
(
  spec
) where

import           Test.Hspec (Spec, describe, it, shouldBe)
import           Block2.Task1 (Expr(..), ArithmeticError(..), eval)

spec :: Spec
spec = describe "Expr tests" $ do
  it "Correctness" $ do
    eval (Const 42) `shouldBe` Right 42
    eval (Add (Const 1) (Const 2)) `shouldBe` Right 3
    eval (Sub (Const 1) (Sub (Const 2) (Const 3))) `shouldBe` Right 2
    eval (Mul (Add (Const (-1)) (Mul (Const 2) (Sub (Const 7) (Const 9)))) (Const 3)) `shouldBe` Right (-15)
    eval (Mul (Add (Const (-1)) (Div (Const 4) (Sub (Const 7) (Const 9)))) (Const 3)) `shouldBe` Right (-9)
    eval (Pow (Const (-2)) (Div (Const 17) (Const 4))) `shouldBe` Right 16
  it "Errors" $ do
    eval (Div (Const 15) (Sub (Const 2) (Const 2))) `shouldBe` Left DivisionByZero
    eval (Pow (Const 1) (Mul (Add (Const (-1)) (Div (Const 4) (Sub (Const 7) (Const 9)))) (Const 3))) `shouldBe` Left NegativePower

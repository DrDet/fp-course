module Task5Spec
( spec
) where

import           Task5      (over, set, (^.), _1, _2)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Pair lens tests" $ do
  let p = (42 :: Int, "Hello")
  it "set" $ do
    set _1 True p `shouldBe` (True, "Hello")
    set _2 True p `shouldBe` (42, True)
  it "view" $ do
    p ^. _1 `shouldBe` 42
    p ^. _2 `shouldBe` "Hello"
  it "over" $ do
    over _1 (Just) p          `shouldBe` (Just 42, "Hello")
    over _2 (++ ", world!") p `shouldBe` (42, "Hello, world!")

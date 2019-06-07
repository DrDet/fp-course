module Task7Spec
( spec
) where

import           Lens.Micro ((^..), (^?))
import           Task7      (FS (..), cd, file, ls)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "FS traverse tests" $ do
  let myDir = Dir "myDir" [Dir "A" [Dir "B" [File "C"]]]
  it "correctness" $ do
    myDir ^?  cd "A" . cd "B" . file "C" `shouldBe` Just "C"
    myDir ^.. cd "A" . cd "B" . ls       `shouldBe` [["C"]]
  it "errors" $ do
    myDir ^?  cd "A" . cd "Baba" . file "C"      `shouldBe` Nothing
    myDir ^.. cd "A" . cd "B" . cd "azazaz" . ls `shouldBe` []

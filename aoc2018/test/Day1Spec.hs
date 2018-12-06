module Day1Spec where

import           Day1       (p1)
import           Test.Hspec

spec :: Spec
spec =
  describe "p1" $
  it "currectly satisfies supplied examples" $ do
    p1 "+1\n+1\n+1" `shouldBe` Just 3
    p1 "+1\n+1\n-2" `shouldBe` Just 0
    p1 "-1\n-2\n-3" `shouldBe` (Just $ -6)

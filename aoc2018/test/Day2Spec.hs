module Day2Spec where

import           Day2       (p1)
import           Test.Hspec

spec :: Spec
spec =
  describe "p1" $
  it "currectly satisfies supplied examples" $ p1 payload `shouldBe` 12
  where
    payload =
      ("abcdef", ["bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"])

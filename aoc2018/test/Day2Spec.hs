module Day2Spec where

import           Day2       (p1, p2)
import           Test.Hspec

spec :: Spec
spec = do
  describe "p1" $
    let payload =
          ( "abcdef"
          , "bababc"
          , ["abbcde", "abcccd", "aabcdd", "abcdee", "ababab"])
     in it "currectly satisfies supplied examples" $ p1 payload `shouldBe` 12
  describe "p2" $
    let payload =
          ("abcde", "fghij", ["klmno", "pqrst", "fguij", "axcye", "wvxyz"])
     in it "currectly satisfies supplied examples" $
        p2 payload `shouldBe` "fgij"

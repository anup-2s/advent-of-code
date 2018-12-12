module Day1Spec where

import           Day1       (p1, p2, readData)
import           Test.Hspec

spec :: Spec
spec = do
  describe "p1" $ do
    it "currectly satisfies supplied examples" $ do
      p1 [1, 1, 1] `shouldBe` 3
      p1 [1, 1, -2] `shouldBe` 0
      p1 [-1, -2, -3] `shouldBe` (-6)
    before readData $
      it "maintains the correct answer" (shouldBe (Right 493) . fmap p1)
  describe "p2" $ do
    it "currectly satisfies supplied examples" $ do
      p2 [1, -1] `shouldBe` Right 0
      p2 [3, 3, 4, -2, -4] `shouldBe` Right 10
      p2 [-6, 3, 8, 5, -6] `shouldBe` Right 5
      p2 [7, 7, -2, -7, -4] `shouldBe` Right 14
    before readData $
      it "maintains the correct answer" (shouldBe (Right 413) . (=<<) p2)

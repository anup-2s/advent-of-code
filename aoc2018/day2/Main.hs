module Main where

import           Data.Text (Text, unpack)
import           Lib       (readDataFile)

data Error =
  NoElementsSupplied
  deriving (Show, Eq)

parse :: Text -> Either Error (String, [String])
parse = safeHd . lines . unpack
  where
    safeHd []     = Left NoElementsSupplied
    safeHd (x:xs) = Right (x, xs)

p1 :: (String, [String]) -> Int
p1 = undefined

main :: IO ()
main = print . fmap p1 . parse =<< readDataFile "data/day2/p1"

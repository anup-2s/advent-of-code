module Main where

import           Control.Arrow ((&&&))
import qualified Data.List     as List
import           Data.Set      (Set, fromList)
import qualified Data.Set      as Set
import           Data.Text     (Text, unpack)
import           Lib           (readDataFile)

data Error =
  NoElementsSupplied
  deriving (Show, Eq)

parse :: Text -> Either Error (String, [String])
parse = safeHd . lines . unpack
  where
    safeHd []     = Left NoElementsSupplied
    safeHd (x:xs) = Right (x, xs)

p1 :: (String, [String]) -> Int
p1 =
  uncurry (*) .
  (count (Set.member 3) &&& count (Set.member 2)) . counts . uncurry (:)
  where
    counts :: [String] -> [Set Int]
    counts = map (fromList . map length . List.group . List.sort)
    count :: (a -> Bool) -> [a] -> Int
    count cond = length . filter cond

main :: IO ()
main = print . fmap p1 . parse =<< readDataFile "data/day2/p1"

module Main where

import           Control.Arrow ((&&&))
import           Data.Either   (rights)
import qualified Data.List     as List
import           Data.Set      (Set, fromList)
import qualified Data.Set      as Set
import           Data.Text     (Text, unpack)
import           Lib           (readDataFile)

data Error
  = NoElementsSupplied
  | OnlyOneElementSupplied
  deriving (Show, Eq)

type MoreThanOne a = (String, String, [String])

safeHdHd []       = Left NoElementsSupplied
safeHdHd [_]      = Left OnlyOneElementSupplied
safeHdHd (x:y:xs) = Right (x, y, xs)

minBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minBy fn xs = snd . minimum . flip zip xs . map fn $ xs

parse :: Text -> Either Error (MoreThanOne String)
parse = safeHdHd . lines . unpack

p2 :: MoreThanOne String -> String
p2 = merge . findBest . computeBests
  where
    penalize :: String -> String -> Int
    penalize str str2 = length . filter (uncurry (==)) $ zip str str2
    best :: String -> String -> [String] -> String
    best _ acc [] = acc
    best from acc (x:xs) = minBy (penalize from) [acc', best from acc' xs]
      where
        acc' = minBy (penalize from) [acc, x]
    computeBests :: MoreThanOne String -> MoreThanOne (String, String)
    computeBests (x, y, xs) = rights . map safeHdHd . tails $ x : y : xs
    findBest :: MoreThanOne (String, String) -> (String, String)
    findBest = undefined
    merge :: (String, String) -> String
    merge = undefined

p1 :: MoreThanOne String -> Int
p1 (x, y, xs) =
  uncurry (*) . (count (Set.member 3) &&& count (Set.member 2)) . counts $
  x : y : xs
  where
    counts :: [String] -> [Set Int]
    counts = map (fromList . map length . List.group . List.sort)
    count :: (a -> Bool) -> [a] -> Int
    count cond = length . filter cond

getData :: IO (Either Error (MoreThanOne String))
getData = parse <$> readDataFile "data/day2/p1"

main :: IO ()
main = print . fmap p1 =<< getData

module Main where

import           Control.Arrow ((&&&))
import qualified Data.List     as List
import           Data.Ord      (comparing)
import           Data.Set      (Set, fromList)
import qualified Data.Set      as Set
import           Data.Text     (Text, unpack)
import           Lib           (printEither, readDataFile)

data Error
  = NoElementsSupplied
  | OnlyOneElementSupplied
  deriving (Show, Eq)

type MoreThanOne a = (a, a, [a])

type AtLeastOne a = (a, [a])

safeHdHd :: [a] -> Either Error (MoreThanOne a)
safeHdHd []       = Left NoElementsSupplied
safeHdHd [_]      = Left OnlyOneElementSupplied
safeHdHd (x:y:xs) = Right (x, y, xs)

minBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minBy fn xs = snd . minimum . flip zip xs . map fn $ xs

parse :: Text -> Either Error (MoreThanOne String)
parse = safeHdHd . lines . unpack

p2 :: MoreThanOne String -> String
p2 = uncurry merge . findBest . makePairs
  where
    choose2 :: [a] -> [(a, a)]
    choose2 xs = do
      (x:xs') <- List.tails xs
      y <- xs'
      return (x, y)
    makePairs :: MoreThanOne String -> AtLeastOne (String, String)
    makePairs (x, y, xs) = (,) (x, y) . drop 1 . choose2 $ x : y : xs
    score :: String -> String -> Int
    score chars = length . filter id . zipWith (/=) chars
    findBest :: AtLeastOne (String, String) -> (String, String)
    findBest = List.minimumBy (comparing $ uncurry score) . uncurry (:)
    merge :: String -> String -> String
    merge chars = map fst . filter (uncurry (==)) . zip chars

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
main = do
  body <- getData
  printEither "Part 1: " $ p1 <$> body
  printEither "Part 2: " $ p2 <$> body
  print "Hi"
  -- printEither "Part 1: " =<< fmap p1 <$> readData
  -- printEither "Part 2: " . (=<<) p2 =<< readData

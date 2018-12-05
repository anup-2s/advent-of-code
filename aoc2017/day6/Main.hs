{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import Paths_aoc2017
import System.Environment (getArgs)
import System.Exit (exitSuccess)

file :: IO FilePath
file = getDataFileName "data/6-1"

getData :: IO [Int]
getData = map read . words <$> (readFile =<< file)

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n -- [--two] [...args]"

data St = St
  { seen :: Set [Int]
  , jumps :: Int
  , blocks :: [Int]
  }

initSt :: [Int] -> Either St St
initSt blocks = Left St {seen = Set.singleton blocks, jumps = 0, blocks = blocks}

maxWithIndex
  :: (Ord b)
  => [b] -> (b, Int)
maxWithIndex = maximum . flip zip [0 ..]

intoList
  :: Foldable b
  => b a -> [a]
intoList = foldr (:) []

{-
- List of numbers
- set ix of max to 0
- update all by (div max 16)
- update take (rem max 16) . drop ix $ cycle [0..16]
- [0 1 2 3 4 5 6]
-          6
- ix - (7 - 6)
-}
perform' :: St -> St
perform' st@St {blocks} =
  let (m, ix) = maxWithIndex blocks
      (d, r) = m `divMod` 16
      updater :: Int -> (Int -> Int)
      updater i
        | i == ix = const d
        | i > ix && i <= (ix + r) = (+) $d + 1
        | i < ix && i <= ix - (16 - r) = (+) $d + 1
        | otherwise = (+ d)
  in st {blocks = map (uncurry ($)) . flip zip blocks $ map updater [0 .. 16]}

check :: St -> Either St St
check St {seen, jumps, blocks} =
  if Set.member blocks seen
    then Right St {seen, blocks, jumps}
    else Left St {seen = Set.insert blocks seen, jumps = jumps + 1, blocks = blocks}

perform :: Either St St -> Int
perform = either (perform . check . perform') jumps

doOne :: [String] -> IO Int
doOne _ = perform . initSt <$> getData

doTwo :: [String] -> IO Int
doTwo _ = undefined <$> getData

parse :: [String] -> IO ()
parse ("--two":xs) = print =<< doTwo xs
parse ["--help"] = usage
parse xs = print =<< doOne xs

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

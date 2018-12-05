{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.Text (split, pack, unpack)
import Paths_aoc2017
import Lib (wordToInt)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

file :: IO FilePath
file = getDataFileName "data/2-1"

getData :: IO [[Int]]
getData = map (map (wordToInt . unpack) . split isSpace . pack) . lines <$> (readFile =<< file)

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n -- [--two] [...args]"

maxMinDiff :: [Int] -> Int
maxMinDiff = uncurry (-) . (maximum &&& minimum)

doOne :: [String] -> IO Int
doOne _ = sum . map maxMinDiff <$> getData

findDivisors :: [Int] -> Int -- partial
findDivisors xs = head [a `div` b | a <- xs, b <- xs, a /= b, a `mod` b == 0]

doTwo :: [String] -> IO Int
doTwo _ = sum . map findDivisors <$> getData

parse :: [String] -> IO ()
parse ("--two":xs) = print =<< doTwo xs
parse ["--help"] = usage
parse xs = print =<< doOne xs

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

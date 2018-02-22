{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (digitToInt)
import Data.Text (strip, pack, unpack)
import Paths_aoc2017
import Lib (rotate)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

file :: IO FilePath
file = getDataFileName "data/1-1"

getData :: IO [Int]
getData = map digitToInt . unpack . strip . pack <$> (readFile =<< file)

formPairs :: Int -> [Int] -> [(Int, Int)]
formPairs n l = zip l $ rotate n l

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n -- [--two] [...args]"

doOne :: [String] -> IO ()
doOne _ = print . sum . map fst . filter (uncurry (==)) <$> formPairs 1 =<< getData

doTwo :: [String] -> IO ()
doTwo _ =
  print . sum . map fst . filter (uncurry (==)) <$> (\l -> formPairs (length l `div` 2) l) =<<
  getData

parse :: [String] -> IO ()
parse ("--two":xs) = doTwo xs
parse ["--help"] = usage
parse xs = doOne xs

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

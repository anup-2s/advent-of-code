{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ((\\), nub, sort)
import Paths_aoc2017
import System.Environment (getArgs)
import System.Exit (exitSuccess)

file :: IO FilePath
file = getDataFileName "data/4-1"

getData :: IO [[String]]
getData = map words . lines <$> (readFile =<< file)

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n -- [--two] [...args]"

hasDupes :: [String] -> Bool
hasDupes xs = null . (\\) xs $ nub xs

doOne :: [String] -> IO Int
doOne _ = length . filter hasDupes <$> getData

hasPermutations :: [String] -> Bool
hasPermutations = hasDupes . map sort

doTwo :: [String] -> IO Int
doTwo _ = length . filter hasPermutations <$> getData

parse :: [String] -> IO ()
parse ("--two":xs) = print =<< doTwo xs
parse ["--help"] = usage
parse xs = print =<< doOne xs

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

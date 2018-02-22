{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((&&&))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Paths_aoc2017
import System.Environment (getArgs)
import System.Exit (exitSuccess)

file :: IO FilePath
file = getDataFileName "data/5-1"

getData :: IO (IntMap Int)
getData = IntMap.fromList . zip [0 ..] . map read . lines <$> (readFile =<< file)

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n [--two] [...args]"

traverseList :: (Int -> Int) -> Int -> IntMap Int -> Int -> Int -> Int
traverseList transform end dict pos jumps
  | pos >= end = jumps
  | pos < 0 = jumps
  | otherwise =
    let cur = dict IntMap.! pos
        pos' = pos + cur
        cur' = transform cur
        dict' = IntMap.insert pos cur' dict
        jumps' = jumps + 1
    in traverseList transform end dict' pos' jumps'

reapp :: a -> b -> (a -> b -> c) -> c
reapp a b f = f a b

perform :: (Int -> Int) -> IntMap Int -> Int
perform transform = reapp 0 0 . uncurry (traverseList transform) . (length &&& id)

doOne :: [String] -> IO Int
doOne _ = perform (+ 1) <$> getData

doTwo :: [String] -> IO Int
doTwo _ = perform transform <$> getData
  where
    transform x | x >= 3 = x - 1
                | otherwise = x + 1

parse :: [String] -> IO ()
parse ("--two":xs) = print =<< doTwo xs
parse ["--help"] = usage
parse xs = print =<< doOne xs

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

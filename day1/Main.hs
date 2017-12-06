{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paths_aoc2017

file :: IO FilePath
file = getDataFileName "data/1-1"

getData :: IO String
getData = readFile =<< file

main :: IO ()
main = print =<< getData

module Lib
  ( rotate
  , wordToInt
  ) where

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop n xs ++ take n xs

wordToInt :: String -> Int
wordToInt = read

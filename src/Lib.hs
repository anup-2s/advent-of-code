module Lib
  ( someFunc
  , rotate
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop n xs ++ take n xs

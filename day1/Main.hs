module Main where

doCaptcha :: [Int] -> Int
doCaptcha _ = 3

main :: IO ()
main = print $ doCaptcha []

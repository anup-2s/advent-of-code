module Main where

import           Data.Text (Text, unpack)
import           Lib       (readDataFile)
import           Text.Read (readMaybe)

conv :: String -> Maybe Int
conv ('+':rest) = readMaybe rest
conv ('-':rest) = (*) (-1) <$> readMaybe rest
conv _          = Nothing

p1 :: Text -> Maybe Int
p1 = fmap sum . mapM conv . lines . unpack

main :: IO ()
main = print =<< (p1 <$> readDataFile "data/day1/p1")

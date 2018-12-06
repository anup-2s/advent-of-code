module Lib where

import           Data.Text     (Text, pack)
import           Paths_aoc2018

readDataFile :: FilePath -> IO Text
readDataFile filePath = pack <$> (readFile =<< getDataFileName filePath)

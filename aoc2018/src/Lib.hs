module Lib where

import           Data.Text       (Text)
import           Data.Text.IO    (readFile)
import           Parsers         (ParseError, Parser)
import           Paths_aoc2018
import           Prelude         hiding (readFile)
import           Text.Megaparsec (runParser)

printEither :: (Show a, Show err) => String -> Either err a -> IO ()
printEither problem err =
  case err of
    Right ans -> putStrLn $ problem ++ show ans
    Left err' -> print err'

readDataFile :: FilePath -> IO Text
readDataFile filePath = readFile =<< getDataFileName filePath

parseDataFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseDataFile parser filePath = do
  fileName <- getDataFileName filePath
  contents <- readFile fileName
  return $ runParser parser fileName contents

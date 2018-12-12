module Main where

import           Control.Applicative        (empty, many, (<|>))
import           Control.Arrow              (left)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Lib                        (parseDataFile)
import           Parsers                    (ParseError, Parser)
import           Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Error
  = NoElementsSupplied
  | ParsingError ParseError
  deriving (Show, Eq)

parser :: Parser [Int]
parser = many (positive <|> negative)
  where
    sc = Lexer.space space1 empty empty
    lexeme = Lexer.lexeme sc
    positive = lexeme (Lexer.symbol sc "+" >> Lexer.decimal)
    negative' = lexeme (Lexer.symbol sc "-" >> Lexer.decimal)
    negative = negate <$> negative'

p1 :: [Int] -> Int
p1 = sum

p2 :: [Int] -> Either Error Int
p2 [] = Left NoElementsSupplied
p2 xs = getRepeatedTotal Set.empty . scanl (+) 0 . cycle $ xs
  where
    getRepeatedTotal :: Set Int -> [Int] -> Either Error Int
    getRepeatedTotal _ [] = Left NoElementsSupplied
    getRepeatedTotal set (val:xss)
      | Set.member val set = Right val
      | otherwise = getRepeatedTotal (Set.insert val set) xss

printAns :: (Show a) => String -> Either Error a -> IO ()
printAns problem err =
  case err of
    Right ans -> putStrLn $ problem ++ show ans
    Left err' -> print err'

readData :: IO (Either Error [Int])
readData = left ParsingError <$> parseDataFile parser "data/day1/p1"

main :: IO ()
main = do
  printAns "Part 1: " =<< fmap p1 <$> readData
  printAns "Part 2: " . (=<<) p2 =<< readData

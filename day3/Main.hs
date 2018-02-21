{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Lib (rotate)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStr, stderr)

getData :: IO Int
getData = return 361527

usage :: IO ()
usage = putStrLn "Usage: stack exec day-n [--two] [...args]"

doOne :: Int -> Int
doOne x =
  let corner = (floor :: Double -> Int) . sqrt $ fromIntegral x
      corner' =
        (if odd corner
           then corner
           else corner - 1)
      side = (corner' - 1) `div` 2
      diff = x - (corner' * corner')
      diff' = diff `rem` corner'
      diff'' = abs (side - diff')
  in diff'' + side

data Move
  = U
  | L
  | D
  | R
  deriving (Show)

type Coord = (Int, Int)

generateRing :: Int -> [Move]
generateRing n =
  concat [[R], replicate n U, replicate (n + 1) L, replicate (n + 1) D, replicate (n + 1) R]

spiral :: [Move]
spiral = concatMap generateRing [1,3 ..]

doMove :: Coord -> Move -> Coord
doMove (x, y) U = (x, y + 1)
doMove (x, y) L = (x - 1, y)
doMove (x, y) D = (x, y - 1)
doMove (x, y) R = (x + 1, y)

buildSpiral :: Int -> [Coord]
buildSpiral n = scanl doMove (0, 0) . take (n - 1) $ spiral

sumNeighbors :: Map Coord Int -> [Coord] -> Int
sumNeighbors prev = sum . map (\k -> Map.findWithDefault 0 k prev)

checkNeighbors :: (Map Coord Int, [Int]) -> (Coord, [Coord]) -> (Map Coord Int, [Int])
checkNeighbors (prev, l) (c, n) =
  let val = sumNeighbors prev n
  in (Map.insert c val prev, val : l)

safeGet
  :: Ord k
  => Map k a -> k -> Maybe a
safeGet m k = Map.findWithDefault Nothing k . Map.map Just $ m

generateNeighborlyValues :: [Coord] -> [Int]
generateNeighborlyValues [] = [1]
generateNeighborlyValues xs =
  reverse .
  snd . foldl checkNeighbors (Map.singleton (0, 0) 1, [1]) . zip xs . map neighbors $ xs

directions :: [Move]
directions = [U, L, D, R]

neighbors :: Coord -> [Coord]
neighbors c = map (doMove c) directions ++ diagonals
  where
    diagonals :: [Coord]
    diagonals = map (\(m1, m2) -> doMove (doMove c m1) m2) . zip directions $ rotate 1 directions

doTwo :: Int -> Either String Int
doTwo m =
  maybe (Left "Not enough spiral") Right .
  Maybe.listToMaybe . filter (> m) . generateNeighborlyValues . tail $
  buildSpiral m

parse :: [String] -> IO ()
parse ["--help"] = usage
parse ["--two"] = either (hPutStr stderr) print =<< doTwo <$> getData
parse [] = print =<< doOne <$> getData
parse _ = usage >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parse args
  exitSuccess

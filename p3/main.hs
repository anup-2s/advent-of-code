import Data.List.Split (chunksOf)
import Data.List

int :: String -> Integer
int x = read x :: Integer

isTriangle :: [Integer] -> Bool
isTriangle [x, y, z] = (x + y) > z

triangle :: [String] -> Bool
triangle x = isTriangle . sort . map int $ x

main = do
  file <- readFile "./input"
  let file' = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"
  let triangles = map words . lines $ file
  let triangles' = concat . map (transpose . reverse) . chunksOf 3 $ triangles
  putStrLn $ (++)"Part 1 " . show . length . filter id . map triangle $ triangles
  putStrLn $ (++)"Part 2 " . show . length . filter id . map triangle $ triangles'

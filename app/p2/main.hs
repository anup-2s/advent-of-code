data Move = U | D | L | R deriving (Enum, Show)

performMove :: Integer -> Move -> Integer
performMove x U
  | x < 4     = x
  | otherwise = x - 3
performMove x D
  | x > 6     = x
  | otherwise = x + 3
performMove x L
  | x `mod` 3 == 1 = x
  | otherwise      = x - 1
performMove x R
  | x `mod` 3 == 0 = x
  | otherwise      = x + 1

convert :: Integer -> Integer
convert x
  | x == 5 = 10
  | x == 1 = 11
  | x == 9 = 12
  | x < 5  = x - 1
  | x < 9  = x - 2
  | x < 13 = x - 3
  | otherwise = x

unconvert :: Integer -> Integer
unconvert x
  | x == 13 = x
  | x == 12 = 9
  | x == 11 = 5
  | x == 10 = 1
  | x <= 3  = x + 1
  | x <= 6  = x + 2
  | otherwise = x + 3

cp :: Integer -> Move -> Integer
cp x d = do
  let c = convert x
  let r = performMove c d
  unconvert r
reg :: Integer -> Bool
reg = (< 10) . convert

performMoveP2 :: Integer -> Move -> Integer
performMoveP2 x U
  | x == 3    = 1
  | reg x     = cp x U
  | x == 13   = 11
  | otherwise = x
performMoveP2 x D
  | x == 11 = 13
  | reg x   = cp x D
  | x == 1  = 3
  | otherwise = x
performMoveP2 x L
  | x == 6 = 5
  | reg x  = cp x L
  | x == 9 = 8
  | otherwise = x
performMoveP2 x R
  | x == 8 = 9
  | reg x  = cp x R
  | x == 5 = 6
  | otherwise = x

toMove :: Char -> Move
toMove 'U' = U
toMove 'D' = D
toMove 'L' = L
toMove 'R' = R

getNum :: Integer -> [Move] -> Integer
getNum = foldl performMove

getNumP2 :: Integer -> [Move] -> Integer
getNumP2 = foldl performMoveP2
getNumP2' :: [Integer] -> [Move] -> [Integer]
getNumP2' xs = do
  let (x:_) = reverse xs
  scanl performMoveP2 x

parseInput :: String -> [[Move]]
parseInput = map (map toMove) . lines

main = do
  file <- readFile "./input"
  let file' = "ULL\nRRDDD\nLURDL\nUUUUD" {- 1985 -}
  let moves = parseInput file
  putStrLn . (++) "Part 1 " . show . drop 1 . scanl getNum 5 $ moves
  putStrLn . (++) "Part 2 " . show . drop 1 . scanl getNumP2 5 $ moves

-- do
-- file <- readFile "./input"
-- -- let file' = "R8, R4, R4, R8"
-- -- let file'' = "R2, L3"
-- putStrLn $ (++) "Part 1: " . show . distFrom . p1 . splitMoves $ file
-- putStrLn $ (++) "Part 2: " . show . distFrom . p2 . splitMoves $ file
 

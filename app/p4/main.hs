module P4(part1, part2)

import Data.List.Split
import Data.List
import Data.Char
import Data.Function((&))

data Room = Room String Int [Char] deriving Show

room :: String -> Room
room x = do
  let code' : sect' : name' = endByOneOf "[-" . reverse $ x
  let [name, sect, code] = map reverse [intercalate " " name', sect', drop 1 code']
  Room name (read sect :: Int) code

freq :: String -> [(Int, Char)]
freq = map(\x -> (length x, head x)) . group . filter (/= ' ') . sort 

sorter :: (Int, Char) -> (Int, Char) -> Ordering
sorter (l, a) (l2, a2)
  | l < l2 = GT
  | l > l2 = LT
  | l == l2 = compare a a2

valid (Room name _ code) =
  name
  & freq
  & sortBy sorter
  & map snd
  & take 5
  & (== code)

sector :: Room -> Int
sector (Room _ s _) = s

rotateBy :: Int -> Char -> Char
rotateBy n ' ' = ' '
rotateBy n c = let 
  base = ord 'a'
  n'   = n `mod` 26
  in chr $ ((ord c - base + n') `mod` 26) + base

shift :: Room -> (String, Int)
shift (Room n s _) = (map (rotateBy s) n, s)

part1 :: String -> Int
part1 = 
foldl (+) 0 . map sector . (map fst . filter snd) . zip rooms . map valid . map room . lines


main = do
  file <- readFile "./input"
  let file' = intercalate "\n" ["aaaaa-bbb-z-y-x-123[abxyz]", "a-b-c-d-e-f-g-h-987[abcde]", "not-a-real-room-404[oarel]", "totally-real-room-200[decoy]", "qzmt-zixmtkozy-ivhz-343[zimth]"]
  let rooms = map room . lines $ file
  let validRooms = map fst . filter snd . zip rooms . map valid $ rooms 
  putStrLn $ (++) "Part 1: " . show . foldl (+) 0 . map sector $ validRooms
  putStrLn . (++) "Part 2: " . show $ snd . head . filter (isInfixOf "north" . fst) . map shift $ validRooms
  -- putStrLn $ (++) "Part 2: " . show . foldl (+) 0 . map shift $ validRooms

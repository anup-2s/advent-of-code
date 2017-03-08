import Data.List.Split
import Data.List
import Data.Function((&))

data Room = Room String Integer [Char] deriving Show
-- room :: String -> [[String]]
room x = do
  let code' : sect' : name' = endByOneOf "[-" . reverse $ x
  let [name, sect, code] = map reverse [concat name', sect', drop 1 code']
  Room name (read sect :: Integer) code

sorter (l, a) (l2, a2)
  | l < l2 = GT
  | l > l2 = LT
  | l == l2 = compare a a2

valid (Room name _ code) =
  name
  & sort
  & group
  & map(\x -> (length x, head x))
  & sortBy sorter
  & map snd
  & take 5
  & (== code)

sector :: Room -> Integer
sector (Room _ s _) = s

main = do
  file <- readFile "./input"
  let file' = intercalate "\n" ["aaaaa-bbb-z-y-x-123[abxyz]", "a-b-c-d-e-f-g-h-987[abcde]", "not-a-real-room-404[oarel]", "totally-real-room-200[decoy]"]
  let rooms = map room . lines $ file
  let validRooms = map fst . filter snd . zip rooms . map valid $ rooms 
  putStrLn $ (++) "Part 1: " . show . foldl (+) 0 . map sector $ validRooms
  -- putStrLn $ (++) "Part 2: " . show . foldl (+) 0 . map sector $ validRooms

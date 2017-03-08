import Data.List.Split
import Data.List

data Room = Room String Integer [Char] deriving Show
-- room :: String -> [[String]]
room x = do
  let code' : sect' : name' = endByOneOf "[-" . reverse $ x
  let [name, sect, code] = map reverse [concat name', sect, drop 1 code']
  Room name (read sect :: Integer) code

main = do
  file <- readFile "./input"
  let file' = intercalate "\n" ["aaaaa-bbb-z-y-x-123[abxyz]", "a-b-c-d-e-f-g-h-987[abcde]", "not-a-real-room-404[oarel]", "totally-real-room-200[decoy]"]
  let rooms = map room . lines $ file'
  print $ take 3 rooms

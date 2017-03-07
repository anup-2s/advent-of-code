import Data.List.Split (splitOn)
import Data.List
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

test = "R1, R2, L1, L2"

data Turn = L | R deriving Show
data Dir = Dir Turn Integer deriving Show

toDir :: [Char] -> Dir
toDir ('R':val) = Dir R (read val::Integer)
toDir ('L':val) = Dir L (read val::Integer)

data Direction = North | East | South| West deriving (Show, Enum)
right :: Direction -> Direction
right West = North
right x = succ x
left :: Direction -> Direction
left North = West
left x = pred x

data Loc = Loc Integer Integer Direction deriving (Show)
instance Eq Loc where
  (==) (Loc x y _) (Loc x' y' _) = x == x' && y == y'

instance Ord Loc where
  (<=) (Loc x y _) (Loc x' y' _) = (x > x') || (y > y')

move :: Loc -> Dir -> Loc
move loc (Dir t s) =
    step s
    . turn t
    $ loc

move' :: [Loc] -> Dir -> [Loc]
move' xs d@(Dir t _) = do
  let (cur : _) = reverse xs
  move'' [] (turn t cur) d
move'' :: [Loc] -> Loc -> Dir -> [Loc]
move'' acc _ (Dir _ 0) = acc
move'' acc l (Dir t s) = do
  let loc = step 1 l
  let ls  = move'' acc loc (Dir t (s - 1))
  loc : ls

turn :: Turn -> Loc -> Loc
turn L (Loc x y d) = Loc x y (left d)
turn R (Loc x y d) = Loc x y (right d)

step :: Integer -> Loc -> Loc
step i (Loc x y East) = Loc (x + i) y East
step i (Loc x y North) = Loc x (y + i) North
step i (Loc x y West) = Loc (x - i) y West
step i (Loc x y South) = Loc x (y - i) South

splitMoves :: String -> [Dir]
splitMoves =
  map (toDir)
  . splitOn ", "

distFrom :: Loc -> Integer
distFrom (Loc x y _) = (abs x) + (abs y)

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s
                        then Just x
                        else dup' xs (Set.insert x s)

stringify :: [Loc] -> [(Integer, Integer)]
stringify = map (\(Loc x y _) -> (x, y))

p1 :: [Dir] -> Loc
p1 = foldl (move) (Loc 0 0 North)
p2 :: [Dir] -> Loc
p2 = 
  head
  . (\x -> (\\) x . nub $ x)
  . concat
  . scanl (move') [(Loc 0 0 North)]


main = do
  file <- readFile "./sample.in"
  -- let file' = "R8, R4, R4, R8"
  -- let file'' = "R2, L3"
  putStrLn $ (++) "Part 1: " . show . distFrom . p1 . splitMoves $ file
  putStrLn $ (++) "Part 2: " . show . distFrom . p2 . splitMoves $ file
 

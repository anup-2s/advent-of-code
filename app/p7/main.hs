#!/usr/bin/env stack
--stack runhaskell
import Data.List
import Data.List.Split
import Debug.Trace (trace)

sw = split . dropBlanks . startsWith

ew = split . dropDelims . dropBlanks . endsWith

brackets :: String -> [String]
brackets = concat . map (ew "]") . sw "["

abba :: String -> Bool
abba ('[':xs) = not $ abba xs
abba (a:b:c:d:xs) = ((a == d) && (a /= b) && (b == c)) || abba (b : c : d : xs)
abba _ = False

doABBA :: ([String], [String]) -> Bool
doABBA (outside, inside) = any abba outside && all abba inside

p1 :: String -> Bool
p1 = doABBA . partition ((/= '[') . head) . brackets

test =
  [ ("abba[mnop]qrst", True)
  , ("abcd[bddb]xyyx", False)
  , ("aaaa[qwer]tyui", False)
  , ("ioxxoj[asdfgh]zxcvbn", True)
  , ( "jgltdnjfjsbrffzwbv[nclpjchuobdjfrpavcq]sbzanvbimpahadkk[yyoasqmddrzunoyyk]knfdltzlirrbypa"
    , False)
  , ("[abcd]abba[bcde]", True)
  ]

main = do
  file <- readFile "./input"
  -- all (uncurry (==)) . zip (map snd test) . map (p1 . fst) $ test ==> MUST BE TRUE
  print $ length . filter id . map p1 $ lines file
  print "Hello"

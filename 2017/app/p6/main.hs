import Data.List
import Data.Ord

freq :: String -> [(Int, Char)]
freq = sortBy (comparing fst) . map(\x -> (length x, head x)) . group . filter (/= ' ') . sort 

main = do
  file <- readFile "./input"
  let file' = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
  print $ (++) "Part 1: " . map (snd . head . reverse .freq ) . transpose . lines $ file
  print $ (++) "Part 2: " . map (snd . head . freq) . transpose . lines $ file

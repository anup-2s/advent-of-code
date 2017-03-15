import qualified Data.Digest.MD5 as MD5
import qualified Data.ByteString.Char8 as Char8(pack, unpack, concat)
import Data.ByteString.Builder(word8Hex, toLazyByteString)
import Data.Char(ord)
import qualified Data.ByteString.Lazy.Char8 as L(concat)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Word
import Data.Bits((.&.))

value :: String -> Int -> String
value s = (++) s . show

bytes :: (Int, String) -> [Word8]
bytes (i, s) = map (fromIntegral . ord) . value s $ i

-- process :: (Int, String
process = MD5.hash . bytes

valid :: [Word8] -> Bool
valid (0:0:x:_)
  | x < 16 = True
  | otherwise = False
valid _ = False

getInput :: [Int] -> String -> [(Int, String)]
getInput i f = zip i . repeat $ f
first8 = take 8 . filter valid
pass (0:0:x:_) = (toLazyByteString . word8Hex) $ x

hashes i = filter valid . map process . getInput i

p1 :: [Int] -> String -> ByteString
p1 i = L.concat . map pass . take 8 . hashes i

main = do
  let file  = "reyedfim"
  let file' = "abc"
  let ints  = [0..]
  let ints' = [3231929, 5017308, 5278568]
  print $ p1 ints file
  

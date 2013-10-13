import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.List.Split as LS
import qualified Data.Bits as Bits
import System.Environment

--cipherCandidates = [['a'..'z'],['a'..'z'],['a'..'z']]
cipherCandidates = [[97..123],[97..123],[97..123]]

main = do
  [fileName] <- getArgs
  fileString <- readFile fileName
  let key = filterCipherCandidates cipherCandidates 0 $ parseWords fileString
--  print $ parseWords fileString
  print $ key
  
  print $ testKey "" (concat key) $ parseWords fileString
  print $ sumFile 0 (concat key) $ parseWords fileString
  
testKey :: String -> [Int] -> [Int] -> String
testKey str _ [] = str
testKey str (k:ks) (char:chars) =
  testKey (str ++ [C.chr $ Bits.xor char k]) (ks++[k]) chars
  
sumFile :: Int -> [Int] -> [Int] -> Int
sumFile mem _ [] = mem
sumFile mem (k:ks) (char:chars) =
  sumFile (mem + (Bits.xor char k)) (ks++[k]) chars
  

parseWords :: String -> [Int]
parseWords str = map read $ LS.splitOn "," str

filterCipherCandidates :: [[Int]] -> Int -> [Int] -> [[Int]]
filterCipherCandidates mem _ [] = mem
filterCipherCandidates cands i (char:chars) = 
  filterCipherCandidates cands' i' chars
  where
    cand = cands !! i
    
    cand' = L.filter (\candDec -> legalAsciiValues $ Bits.xor char candDec) cand
    
    (ch, ct) = L.splitAt i cands
    cands' = ch ++ (cand':tail ct)
    
    i' = if i == 2
         then 0
         else i + 1
    

legalAsciiValues :: Int -> Bool
legalAsciiValues value
  | value < 14 = True
  | value > 27 &&
    value < 91 = True
  -- space, exclamation mark, hyphen
  | value > 31 &&
    value < 34 = True
-- dot, comma
  | value == 44 ||
    value == 46 = True
-- A..Z  
  | value > 64 &&
    value < 91 = True
-- a..z
  | value > 96 &&
    value < 123 = True
                  
  | otherwise = False


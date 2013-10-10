import qualified Data.Digits as Digits
import qualified Data.List as L
digits = [0..9]


possibleNumbers :: [(Int, [Int])]
possibleNumbers = map (\ds -> (Digits.unDigits 10 ds, ds)) $ L.permutations digits

solve = L.foldl' (\acc n -> acc + fst n) 0 $ filter subStringDivisable possibleNumbers

subStringDivisable :: (Int, [Int]) -> Bool
subStringDivisable (n,digits) = all (\(prime,n) -> mod n prime == 0) pSubs
  where
    pSubs = zip [2,3,5,7,11,13,17] $ map (Digits.unDigits 10) $ subStrings 3 $ tail digits
    
subStrings :: Int -> [Int] -> [[Int]]
subStrings l string
  | length string >= l = let (hd,tl) = splitAt l string
                         in hd:subStrings l (tail hd ++tl)
  | otherwise = []

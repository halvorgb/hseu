import Data.List
import Data.Ord

showFrac :: Double -> String
showFrac n = tail $ tail $ show (1/n)

solve :: Double -> Double
solve n = solve' [1..n] 0 1

solve' :: [Double] -> Int -> Double -> Double
solve' [] _ i = i
solve' (x:xs) l i
  | l' > l    = solve' xs l' x
  | otherwise = solve' xs l i
  where
    l' = longestRepeatingSubStr $ showFrac x




--reciprocalCycleLength :: String -> Int -> Int


longestRepeatingSubStr :: String -> Int
longestRepeatingSubStr str = checkAllSubStrings subStrings 1
  where
    subStrings = filter (\l -> length l > 1) $ subsequences str

-- I don't understand this problem.

checkAllSubStrings :: [String] -> Int -> Int
checkAllSubStrings [] l = l
checkAllSubStrings (ss:subStrings) l =
  checkAllSubStrings misses (max l l')
  where
    l' = length hits
    (hits, misses) = partition (== ss) subStrings
    
problem_26 = fst $ maximumBy (comparing snd)
                            [(n,recurringCycle n) | n <- [1..999]]
    where  recurringCycle d = remainders d 10 []
           remainders d 0 rs = 0
           remainders d r rs = let r' = r `mod` d
                               in case elemIndex r' rs of
                                    Just i  -> i + 1
                                    Nothing -> remainders d (10*r') (r':rs)

import Data.List

showFrac :: Double -> String
showFrac n = tail $ tail $ show (1/n)

solve :: Double -> Int
solve n = solve' [1..n] 0

solve' :: [Double] -> Int -> Int
solve' [] m = m
solve' (x:xs) m
  | l > m = solve' xs l
  | otherwise = solve' xs m
  where
    l = longestRepeatingSubStr $ showFrac x

longestRepeatingSubStr :: String -> Int

longestRepeatingSubStr str = checkAllSubStrings str (generateAllSubStrings str) 0

generateAllSubStrings :: String -> [String]
generateAllSubStrings str = filter (\x -> length x > 0) (filter (\x -> (length x <= length str)) $  nub $ subsequences str)

checkAllSubStrings :: String -> [String] -> Int -> Int

checkAllSubStrings _ [] m = m
checkAllSubStrings str (s:ss) m
  | (happensTwice str s False) && length s > m = checkAllSubStrings str ss (length s)
  | otherwise = checkAllSubStrings str ss m
                

happensTwice :: String -> String -> Bool -> Bool

happensTwice [] _ _ = False
happensTwice str@(s:ss) substr found
  | happenCheck && found == True = True
  | happenCheck = happensTwice (drop (length substr) str) substr True
  | otherwise = happensTwice ss substr found
  where
    happenCheck = (take (length substr) str )== substr
    


-- isInfixOf substr str -> bool
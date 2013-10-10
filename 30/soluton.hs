import Data.List

upperB = 354294
lowerB = 2

answer = foldl' digitFifthPowers 0 [lowerB..upperB]


digitFifthPowers :: Int -> Int -> Int
digitFifthPowers acc n
  | n == (sum $ map (^5) $ digits n) = acc + n
  | otherwise = acc

digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)
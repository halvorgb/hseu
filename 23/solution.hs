main = do 
  putStrLn $ show answer
  putStrLn "^---"

abundantNumbers = [x |x <- [3..28123], x < getDivisorSum x]
answer = foldl1 (+) [y | y <- [1..28123], cantBeGenerated y]

cantBeGenerated :: Int -> Bool

-- finds all pairs of possible generator of n, we only need to find  1 or 0.
-- cantBeGenerated n = filter (\x -> elem (n-x) abundantNumbers) (takeWhile (<n) abundantNumbers) == []

cantBeGenerated n = cantBeGenerated' (takeWhile (<n) abundantNumbers) n

cantBeGenerated' :: [Int] -> Int -> Bool

cantBeGenerated' [] _ = True
cantBeGenerated' l@(x:xs) n
  | x > (div n 2) = True
  | elem (n-x) l = False
  | otherwise = cantBeGenerated' xs n


-- initial call: getDivisors Int
getDivisorSum :: Int -> Int
getDivisorSum n  = foldl1 (+) [x | x <- [1..((div n 2)+1)], n `mod` x == 0]
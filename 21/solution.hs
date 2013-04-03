main = do

  putStrLn $ show $ sumAmicable 10000
  

{-

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

-}

sumAmicable :: Int -> Int

-- sumAmicable n = foldl (\acc x -> (fst x) + acc) 0 (filter (\x -> 
--                                                            if (snd x) > n then False
--                                                            else (fst (divisorlist !! (snd x)) == (fst x))) divisorlist)
sumAmicable n = foldl (\acc x -> (fst x) + acc) 0 (filter (\x -> if (snd x) > n then False else (intDivisorPairAtIndex (snd x) (fst x) divisorlist)) divisorlist)
  where
    divisorlist = divisorList n

intDivisorPairAtIndex :: Int -> Int -> [(Int, Int)] -> Bool
intDivisorPairAtIndex _ _ [] = False
intDivisorPairAtIndex i o ((index, sum):rest)
  | ((i == index) && (o /= index) && (o == sum)) = True
  | otherwise = intDivisorPairAtIndex i o rest

divisorList :: Int -> [(Int, Int)]
divisorList n
  | n == 0 = []
  | otherwise = (n, getDivisorSum n):divisorList (n-1)


-- initial call: getDivisors Int
getDivisorSum :: Int -> Int
getDivisorSum n  = foldl1 (+) [x | x <- [1..((div n 2)+1)], n `mod` x == 0]


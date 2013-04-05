minBound = (-999)
maxBound =   999


main = do
  putStrLn $ show answer
  
answer :: Int  
answer = 1



--fukdis
lazyPrimes :: [Int]
lazyPrimes = primes 9999

primes :: Int -> [Int]
-- BEIS CASE
primes 2 = [2]
primes x
  | even x = memoizedPrimes
  | not $ any (\y -> mod x y == 0) memoizedPrimes = x:memoizedPrimes
  | otherwise = memoizedPrimes
  where
    memoizedPrimes = primes (x-1)
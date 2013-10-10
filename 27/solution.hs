import Data.Maybe

import Data.Numbers.Primes as Primes

minAB = -999
maxAB =  999


main = do
  putStrLn $ show $ answer
  

answer = runA minAB Primes.primes (0,0,0)

type ABN = (Int, Int, Int)

runA :: Int -> [Int] -> (Int,Int,Int) -> (Int, Int, Int)
runA a primes m@(_, _, rec)
  | a > maxAB = m
  | otherwise = runA (a+1) primes' m'
  where
    m' =
      if rec' > rec
      then (a, bC, rec')
      else m
    (bC, rec', primes') = runB a minAB primes (0,0)
    

runB :: Int -> Int -> [Int] -> (Int, Int) -> (Int, Int, [Int])
runB a b primes m@(bC, rec)
  | b > maxAB = (bC, rec, primes)
  | otherwise = runB a (b+1) primes' m'
  where
    m' = if rec' > rec
         then (b, rec')
         else m
    (rec', primes') = countPrimes 0 a b primes
    

countPrimes :: Int -> Int -> Int -> [Int] -> (Int, [Int])
countPrimes n a b primes =
  if isPrime
  then countPrimes (n+1) a b primes
  else (n, primes)
  where
    res = n^2 + a*n + b
    isPrime = checkPrime res primes

checkPrime :: Int -> [Int] -> Bool
checkPrime num ps@(prime:primes)
  | num < prime = False
  | num > prime = checkPrime num primes
  | num == prime = True
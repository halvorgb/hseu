import qualified Data.Numbers.Primes as Primes
import qualified Data.List as L

maxVal = 1000000
millionPrimes = takeWhile (<10000) Primes.primes
subSeqs = concatMap L.inits . L.tails

solve = L.foldl' longestConsecPrime (0,0) $ subSeqs millionPrimes

longestConsecPrime :: (Int, Int) -> [Int] -> (Int, Int)
longestConsecPrime mem [] = mem
longestConsecPrime mem@(maxCount, maxSum) primes 
  | count' < maxCount ||
    sum' > maxVal ||
    (not $ isPrime) = mem
  | otherwise = mem'
  where
    isPrime = Primes.isPrime sum'
    count' = length primes
    mem' = if count' > maxCount
           then (count', sum')
           else mem
    sum' = sum primes


{-
consecutivePrimes :: [Int] -> [Int] -> Int -> Int -> (Int,Int) -> (Int, Int)
consecutivePrimes [] _ _ _ mem = mem
consecutivePrimes (prime:primes) [] _ _ mem = consecutivePrimes primes primes 0 0 mem
consecutivePrimes ps@(prime:primes) (nextPrime:nextPrimes) count sum mem@(maxCount, maxSum)
  | not $ Primes.isPrime sum' || 
    sum' > 1000000 = consecutivePrimes primes primes 0 0 mem
  | nextPrime == prime = consecutivePrimes primes nextPrimes count' sum' mem'
  | otherwise          = consecutivePrimes ps nextPrimes count' sum' mem'
  where
    mem' = if count' > maxCount
           then (count', sum')
           else mem
    count' = count + 1
    sum' = sum + nextPrime

-}
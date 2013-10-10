import qualified Data.Numbers.Primes as Primes
import qualified Data.Digits as Digits

import qualified Data.List as L

solve = rotationalPrimes primes 2 0
  where
    primes = Primes.primes

rotationalPrimes :: [Int] -> Int -> Int -> Int
rotationalPrimes primes 1000000 acc = acc
rotationalPrimes primes n acc
  | all (\r -> r > 1 &&
               Primes.isPrime r) rotationalNums  = rotationalPrimes primes (n+1) acc+1
  | otherwise = rotationalPrimes primes (n+1) acc
  where
    nDigs = Digits.digits 10 n
    rotationalNums = map (Digits.unDigits 10) $ rotations nDigs
    

-- teit løsning.
rotations :: [Int] -> [[Int]]
rotations digits =
  L.unfoldr (\k -> if k == 0
                   then Nothing
                   else Just (rotate k digits, k-1)) l
  where
    l = length digits
    

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
import qualified Data.Numbers.Primes as Primes
import qualified Data.List as L

solve = findFourConsecutive [1..]

findFourConsecutive :: [Int] -> Int
findFourConsecutive list
  | all (\fs -> length fs == 4) factors = head list
  | otherwise = findFourConsecutive $ tail list
  where
    firstFour = L.take 4 list
    factors :: [[Int]]
    factors = L.map (L.nub . Primes.primeFactors) firstFour

  
    
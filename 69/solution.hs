import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes

solve :: Int -> Int
solve n = totientMaximum [1..n] (takeWhile (< 1000000) Primes.primes) 0.0

totientMaximum :: [Int] -> [Int] -> Double -> Int
totientMaximum (n:ns) primes mem = n


bjarne = fold
import qualified Data.Numbers.Primes as Primes
import qualified Data.List as L

solve = tenPercentPrimes 2 1 0 1

tenPercentPrimes :: Int -> Int -> Int -> Int -> Int
--tenPercentPrimes 10000 _ p n = error $ show p ++", " ++ show n
tenPercentPrimes spiralNum previousNum diagonalPrimes diagonalNumbers
  | tenCheck = 2 * spiralNum - 1
  | otherwise = tenPercentPrimes (spiralNum + 1) lastNum diagonalPrimes' diagonalNumbers'
  where
    -- implement running average? prone to rounding imprecision?
    tenCheck = (fromIntegral diagonalPrimes' / (fromIntegral diagonalNumbers')) < 0.1
    
    
    diagonalNumbers' = diagonalNumbers + 4
    (spiralCorners', lastNum) =  spiralCorners spiralNum previousNum
    diagonalPrimes' = (length $ L.filter Primes.isPrime spiralCorners')
                      + diagonalPrimes
                      
spiralCorners :: Int -> Int -> ([Int], Int)
spiralCorners spiralNum previousNum =
  ([previousNum + add, 
    previousNum + add + add,
    previousNum + add + add + add,
    previousNum + add + add + add + add
   ],
    previousNum + add + add + add + add
   )
  where
    -- the Nth even number.
    add = 2 * (spiralNum-1)

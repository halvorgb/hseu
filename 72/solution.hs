module Main where
import System.Environment

import qualified Data.Numbers.Primes as Primes
import qualified Data.List as L


main = do
  [n] <- getArgs
  putStrLn $ show $ solve $ read n

solve :: Int -> Int
solve n = L.foldl' (\acc divisor -> acc + redPropFracs divisor n) 0 [2..n]

redPropFracs :: Int -> Int -> Int
redPropFracs d maxD
    | Primes.isPrime d = (d-1)
    | d * 2 > maxD = lesserFracs d
    | otherwise = 2

lesserFracs :: Int -> Int
lesserFracs d = L.length $ L.filter (\n -> gcd n d == 1) [1..(d-1)]



--brute
-- solve n = L.foldl' (\acc divisor -> acc + reducedProperFractions divisor [1..(divisor-1)]) 0 [2..n]

-- reducedProperFractions :: Int -> [Int] -> Int
-- reducedProperFractions x ys =
--     L.length $ L.filter (\y -> gcd x y == 1) ys




{-

----    1/2
--- 1/3    2/3
-- 1/4   .   3/4
- 1/5 2/5 3/5 4/5
1/6 . . . . . . 5/6
1/7 2/7 3/7 4/7 5/7 6/7
1/8 . 3/8 . 5/8 . 7/8
1/9 2/9 . 4/9 . 5/9 . 7/9 . 8/9







 ..   ..   v    ..    v     v   ..   ..   v     v    ..     v    ..    ..
1/15 2/15 3/15 4/15  5/15 6/15 7/15 8/15 9/15 10/15 11/15 12/15 13/15 14/15

-}

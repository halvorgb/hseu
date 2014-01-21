module Main where

import Data.Ratio
import qualified Data.List as List
{-
        1/2
      1/3 2/3
    1/4 2/4 3/4
   1/5 2/5 3/5 4/5
  1/6 2/6 3/6 4/6 5/6
 1/7 2/7 3/7 4/7 5/7 6/7

-}

--2/5 < n/d < 3/7

-- n/d < 3/7


solve :: Int -> Ratio Int
solve n = List.foldl' (\acc i ->
                           let x = largestRatio bound i
                           in max x acc
                       ) (0%1) [1..n]
  where
    bound = 3 % 7

largestRatio :: Ratio Int -> Int  -> Ratio Int
largestRatio bound d
  | nd == bound = (n-1)%d -- remove multiples of bound
  | otherwise = nd
  where
    nd = n%d
    n = floor $ (fi d) * (fi $ numerator bound) / (fi $ denominator bound)

fi = fromIntegral

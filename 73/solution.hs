import Data.Ratio
import qualified Data.List as List

solve :: Int -> Int
solve n = List.foldl' (\acc i ->
                        let x = countFracs lowerbound upperbound i
                        in acc + x
                      ) 0 [1..n]
  where
    upperbound = 1 % 2
    lowerbound = 1 % 3

--suboptimal..? brutefaen
countFracs :: Ratio Int -> Ratio Int -> Int -> Int
countFracs lowerbound upperbound d =
  List.length $ List.filter (\n -> gcd n d == 1 &&
                                   n % d < upperbound &&
                                   n % d > lowerbound ) [minN..maxN]
  where
    minN = ceiling $ (fi d) * (fi $ numerator lowerbound) / (fi $ denominator lowerbound)
    maxN = floor $ (fi d) * (fi $ numerator upperbound) / (fi $ denominator upperbound)


fi = fromIntegral
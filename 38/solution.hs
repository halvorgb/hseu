import qualified Data.Digits as Digits
import qualified Data.List as L

upperBound = 99999

digits = [1,2,3,4,5,6,7,8,9]
-- n * 1 = n
-- n * 2 = 2n => n digits ++ 2n digits == 9, ergo n digits < 5

solve =  L.foldl' panDigitalProduct 1 [1..upperBound]

panDigitalProduct :: Int -> Int -> Int
panDigitalProduct acc i =
  max acc $ panCheck 0 i 1

panCheck :: Int -> Int -> Int -> Int
panCheck 0 x 1 = panCheck x x 2
panCheck sum x n
  | sDigits /= L.nub sDigits = 0
  | elem 0 sDigits = 0
  | length sDigits < 9 = let sum' = Digits.unDigits 10 $ sDigits ++ Digits.digits 10 (x*n)
                         in panCheck sum' x n+1
  | length sDigits == 9 = sum
  | otherwise = 0
    
  where
    sDigits = Digits.digits 10 sum
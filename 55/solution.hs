import qualified Data.Digits as Digits
import qualified Data.List as L

solve = length $ filter (isLychrelNumber 50 50) [1..9999]

isLychrelNumber :: Integer -> Integer -> Integer -> Bool
isLychrelNumber maxIterations iterations number
  | iterations == 1 = True
  | iterations /= maxIterations &&
    isPalindrome = False
  | otherwise = isLychrelNumber maxIterations iterations' number'
  where
    iterations' = iterations - 1
    number' = number + reverseNumber
    isPalindrome = reverseNumber == number
    reverseNumber = Digits.unDigits 10 $ reverse digits
    digits = Digits.digits 10 number
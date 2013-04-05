import Data.Char

{- 
n! means n  (n  1)  ...  3  2  1

For example, 10! = 10  9  ...  3  2  1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial i = i * factorial (i-1)

sumDigits :: Integer -> String
sumDigits n = show $ foldl (+) 0 (map digitToInt (show (factorial n)))

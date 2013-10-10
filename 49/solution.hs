import qualified Data.Numbers.Primes as Primes
import qualified Data.Digits as Digits
import qualified Data.List as L
import qualified Data.Maybe as Maybe

fourDigitPrimes = takeWhile (<10000) $ dropWhile (<1000) Primes.primes

solve = findSequence fourDigitPrimes

findSequence :: [Int] -> (Int, Int, Int)
findSequence [] = (0,0,0)
findSequence (1487:primes) = findSequence primes -- HHhhHh  
findSequence (a:primes) = case cs of 
  Just ns -> ns
  _ -> findSequence primes
  where
    cs = L.foldl' (\p b -> case p of
                      Just _ -> p
                      _ -> let c = b + (b-a)
                           in if Primes.isPrime c && isPalindrome a c
                              then Just (a, b, c)
                              else Nothing
                  ) Nothing bs
    bs = L.filter (\b -> isPalindrome a b) primes
    




isPalindrome :: Int -> Int -> Bool
isPalindrome x y =
  all (\xd -> elem xd yDigits) xDigits &&
  all (\yd -> elem yd xDigits) yDigits && 
  length xDigits == length yDigits
  where
    xDigits = Digits.digits 10 x 
    yDigits = Digits.digits 10 y 
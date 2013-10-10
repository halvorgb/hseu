import Data.Maybe
import Data.Digits
import Data.List

minA = 1
maxA = 99
minB = 98
maxB = 6789

digitsToHit = [1..9]

solution = runA minA []

runA :: Int -> [Int] -> [Int]
runA a mem
  | a > maxA = mem
  | otherwise = runA (a+1) mem'
  where
    mem' = runB a minB mem
    
runB :: Int -> Int -> [Int] -> [Int]
runB a b mem
  | b > maxB = mem
  | otherwise = runB a (b+1) mem'
  where
    mem' = case nineDigits a b of
           Just n -> n:mem
           _ -> mem
                
                

nineDigits :: Int -> Int -> Maybe Int
nineDigits n m
  | length allDigits == 9 &&
    null (digitsToHit \\ allDigits) = Just nm
  | otherwise = Nothing
  where
    nm = n*m
    allDigits = nDigits ++ mDigits ++ nmDigits
    nDigits = digits 10 n
    mDigits = digits 10 m
    nmDigits = digits 10 nm
    
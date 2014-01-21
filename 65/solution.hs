import Data.Ratio
import System.Environment
import Data.Digits
import Data.List
main = do
  [n] <- getArgs
  print $ solve $ read n

solve :: Int -> Integer
solve n = sum $ digits 10 $ numerator $ convergent values
  where
    values = 2:(take (n-1) $ concatMap (\i -> [1, 2*i, 1]) [1..])

convergent :: [Rational] -> Rational
convergent (x:xs)
  | null xs = x
  | otherwise = x + (1 / convergent xs)
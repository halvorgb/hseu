module Main where
import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
import qualified Data.Digits as Digits
import Data.Maybe
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  putStrLn $ show $ solve $ read n

solve :: Int -> (Int, Double)
solve n = totientMaximum 2 n (0, 99999999999999999)

totientMaximum :: Int -> Int -> (Int, Double) -> (Int, Double)
totientMaximum n top mem@(m, mx)
    | n > top = mem
    | totient /= 0 &&
      (isPermutationOf n totient) =
          totientMaximum n' top mem'
    | otherwise = totientMaximum n' top mem
    where
      facs = L.nub $ Primes.primeFactors n

      totient
          | null facs = 0
          | otherwise =
              round $ fin * (L.foldl' (\acc fac ->
                                           acc * (1.0 - 1/(fromIntegral fac))
                                      ) 1.0 facs)
      fin :: Double
      fin = fromIntegral n
      n' = n + 1

      tt = fin/(fromIntegral totient)
      mem' = if tt < mx
             then (n, tt)
             else mem


isPermutationOf :: Int -> Int -> Bool
isPermutationOf a b = ad == bd
    where
      ad = L.sort $ Digits.digits 10 a
      bd = L.sort $ Digits.digits 10 b

import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  return ()

solve :: Int -> (Int, Double)
solve n = totientMaximum 2 n (0, -0.0)

totientMaximum :: Int -> Int -> (Int, Double) -> (Int, Double)
totientMaximum n top mem@(m, mx)
    | n > top = mem
    | otherwise = totientMaximum n' top mem'
    where
      facs = L.nub $ Primes.primeFactors n

      totient
          | null facs = fin - 1
          | otherwise =
              fin * (L.foldl' (\acc fac ->
                               acc * (1.0 - 1/(fromIntegral fac))
                              ) 1.0 facs)
      fin :: Double
      fin = fromIntegral n
      n' = n + 1

      tt = fin/totient
      mem' = if tt > mx
             then (n, tt)
             else mem

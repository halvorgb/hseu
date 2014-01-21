import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
import System.Environment

main :: IO ()
main = do
  [n, m] <- getArgs

  return ()

solve :: Integer -> Integer -> Integer
solve n m = totientSum n nFacs nTotient 1 m 0
    where
      nFacs = L.nub $ Primes.primeFactors n
      nTotient :: Integer
      nTotient = n * (round $  L.foldl' (\acc fac ->
                                             acc * (1.0 - 1.0/(fromIntegral fac))
                                        ) 1.0 nFacs)

totientSum :: Integer -> [Integer] -> Integer -> Integer -> Integer -> Integer -> Integer
totientSum n nFacs nTotient i m mem
    | i == m = mem'
    | otherwise = totientSum n nFacs nTotient (i+1) m mem'
    where
      iFacs = L.delete i $ Primes.primeFactors i
      newFacs = iFacs L.\\ nFacs

      totient :: Integer
      totient = nTotient * i * (round $ L.foldl' (\acc fac ->
                                          acc * (1.0 - 1.0/(fromIntegral fac))

                                         ) 1.0 newFacs)

      -- totient :: Double
      -- totient = fromIntegral (n*i) * (L.foldl' (\acc fac ->
      --                                             acc * (1.0 - 1.0/(fromIntegral fac))
      --                                        ) 1.0 nmFacs)
      mem' = mem + totient

testSpeed :: Integer -> Integer -> Integer -> Integer
testSpeed n max mem
    | n == max = mem'
    | otherwise =  testSpeed (n+1) max mem'
    where
      mem' = mem + 1 --(L.sum $ Primes.primeFactors n)

testSpeed2 :: Integer -> Int
testSpeed2 n = L.length $ takeWhile (<n) Primes.primes

import qualified Data.Numbers.Primes as Primes
import qualified Data.List as L


solve = L.find goldbachWrong [x | x <- [9..], odd x]

goldbachWrong :: Int -> Bool
goldbachWrong x 
  | Primes.isPrime x = False
  | otherwise = not $ testConjecture x
                       
testConjecture :: Int -> Bool
testConjecture x = L.any (\prime ->
                           any (\base -> 
                                 prime + 2*base^2 == x
                               ) testSquareBases
                         ) testPrimes
  where
    testSquareBases = [1..round $ sqrt (fromIntegral x)]
    testPrimes = takeWhile (<x) Primes.primes



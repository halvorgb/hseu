-- http://en.wikipedia.org/wiki/Farey_sequence

import qualified Data.Numbers.Primes as Primes
import qualified Data.List as List
-- f_n = 1 + sum totient(m), where 0 < m <= n

fareyN :: Integer -> Integer
fareyN n = (-2) + List.foldl' (\acc m -> acc + totient m ) 1 [1..n]


totient :: Integer -> Integer
totient n = round $ List.foldl' (\acc prime ->
                                  acc * (1.0 - 1 / (fi prime))
                                ) (fi n) pFs
  where
    pFs = List.nub $ Primes.primeFactors n

fi = fromIntegral

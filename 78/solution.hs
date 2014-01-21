import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import Data.Numbers.Primes

-- | http://www.mathpages.com/home/kmath383.htm
-- | ^ algorithm

--solve :: Integer -> Integer
solve n = L.foldl' (\pm i -> p i pm sm) M.empty [0..n]
    where
      sm = L.foldl' (\m k -> M.insert k (s k) m) M.empty [1..n]


p :: Integer -> M.Map Integer Integer -> M.Map Integer Integer -> M.Map Integer Integer
p 0 pm _  = M.insert 0 1 pm
p n pm sm = M.insert n (div pn n) pm
    where
      ks = [1..n]
      pn = L.foldl' (\acc k ->
                     acc +
                     (Mb.fromJust $ M.lookup k sm) *
                     (Mb.fromJust $ M.lookup (n-k) pm)
                    ) 0 ks



-- | Sum of Divisors
-- Korrekt.
s :: Integer -> Integer
s 0 = 0
s 1 = 1
s k = L.foldl (\acc (p,a) -> acc * div (p^(a+1) - 1) (p - 1)) 1 primesToTheNth
    where
      (fst:factors) = primeFactors k
      primesToTheNth = f fst 1 factors

      f :: Integer -> Integer -> [Integer] -> [(Integer, Integer)]
      f factor count [] = [(factor, count)]
      f factor count (factor':factors)
          | factor == factor' = f factor (count + 1) factors
          | otherwise         = (factor, count):(f factor' 1 factors)

import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
import qualified Data.Digits as Digits
import qualified Data.Map as M
import qualified Data.Maybe as Mb

millionPrimes :: ([Integer], M.Map Integer Integer)
millionPrimes = (primes, M.fromList $ zip primes primes)
  where
    primes = take 10000000 Primes.primes

solve n = findNFamily n (-1) (-1, []) [] primeList primeMap
  where
    (primeList, primeMap) = millionPrimes


findNFamily ::Int -> Integer -> (Integer, [Integer]) -> [(Integer, [Int])] -> [Integer] -> M.Map Integer Integer-> (Integer, [Integer])
findNFamily _ _ _ _ [] _ = error "out of primes"
findNFamily goal bounds mem@(minSum, _) visitedPrimes (prime:primes) m
  | bounds /= -1 &&
    prime > bounds = mem
--  | prime > 1000 = error $ "missed base case" ++ show mem
  | (L.length candidatePrimes') == (goal - 1) = --error "lol"
    let summedPrimes = prime:(L.map fst candidatePrimes')
        sum' = sum summedPrimes
      in if minSum == (-1) ||
            minSum > sum'
         then     findNFamily goal sum' (prime, summedPrimes) ((prime, digs):visitedPrimes) primes m
         else     findNFamily goal bounds mem                 ((prime, digs):visitedPrimes) primes m
  | otherwise = findNFamily goal bounds mem                 ((prime, digs):visitedPrimes) primes m
  where
    digs = integerToDigits prime
    candidatePrimes = L.filter (\(_, digs') ->
                                 oneToOnePrime m digs digs'
                               ) visitedPrimes
    
    lesserPrimeLength = length candidatePrimes
    candidatePrimes'
      | lesserPrimeLength >= goal' =
        if lc' == goal'
        then if allToAllPrime (map snd cands') m
             then cands'
             else []
        else if lc' > goal'
             then let cands'' :: [[(Integer, [Int])]]
                      cands'' = L.filter (\sbsq -> (length sbsq == goal') &&
                                                   (allToAllPrime (map snd sbsq) m)
                                         ) $ L.subsequences cands'
                  in if (not $ null cands'')
-- debug                     then error $ show prime ++ show candidatePrimes ++ foldl (\prev subseq -> prev ++ show (map snd subseq) ++ ", ") "" cands''
                        -- the lowest sum 
                     then L.foldl1' (\candidate candidate' -> let s = sum $ map fst candidate
                                                                  s' = sum $ map fst candidate'
                                                              in if s < s'
                                                                 then  candidate
                                                                 else candidate') cands''
                     else []
             else []
      | otherwise = []
      where
        goal' = goal-1
        lc' = L.length cands'

        cands' :: [(Integer, [Int])]
        cands' = L.filter (\(_, digs') ->
                   nTimesTrue (oneToOnePrime m) (goal-2) (map snd candidatePrimes) digs'
                 ) candidatePrimes
                 
oneToOnePrime :: M.Map Integer Integer -> [Int] -> [Int] -> Bool
oneToOnePrime m digs digs' =
  digs /= digs' &&
  Mb.isJust (M.lookup n m) &&
  Mb.isJust (M.lookup n' m)
  where
    n = digitsToInteger (digs ++ digs')
    n' = digitsToInteger (digs' ++ digs)
    
allToAllPrime :: [[Int]] -> M.Map Integer Integer -> Bool
allToAllPrime [] _ = True
allToAllPrime (primeD:primeDs) m
  | all (\primeD' -> oneToOnePrime m primeD primeD') primeDs
    = allToAllPrime primeDs m
  | otherwise = False


-- ^ Bit poorly named, analogous to a filter (any n times).
nTimesTrue :: (a -> a -> Bool) -> Int -> [a] -> a -> Bool
nTimesTrue f n bs a = n' >= n -- n' == n ?
  where
    n' = L.length $ L.filter (f a) bs 
    
integerToDigits :: Integer -> [Int]
integerToDigits = Digits.digits 10 . fromIntegral

digitsToInteger :: [Int] -> Integer
digitsToInteger = Digits.unDigits 10 . map fromIntegral

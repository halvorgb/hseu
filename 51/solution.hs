import qualified Data.Digits as Digits
import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
import Debug.Trace
{-
x er variabel, n er konstant.
10 -> 99:
nx, xn
100 -> 999
nnx, nxx, xxn, xnn, xnx




-}

type Digit = Int


primes :: [Int]
primes = dropWhile (<13) Primes.primes

--solve = L.find (replacedPrimes 8) primes
solve n = replacedPrimes n primes (0, [])

replacedPrimes :: Int -> [Int] -> (Int, [[Int]]) -> Int
replacedPrimes goal (prime:primes) ind@(oldL, indices)
  | any (\primeNums' -> length primeNums' == goal) primeNums = prime
  | prime == 121313 && 
    trace ("nonLZ " ++ show nonLeadingZero  ++ "\n pN " ++ show primeNums) True = error ""
  | otherwise = replacedPrimes goal primes ind'
  where
    

    primeNums :: [[Bool]]
    primeNums = map (\nonLZ' -> filter (==True) $ map (Primes.isPrime . Digits.unDigits 10) nonLZ') nonLeadingZero
    nonLeadingZero :: [[[Digit]]]
    nonLeadingZero = map (filter (\digs -> digs !! 0 /= 0)) $ replaceDigits digs indices''
    
    indices'' = filterIndices digs indices'
    ind'@(_, indices') = if digL > oldL
                            then (digL, createIndices digL)
                            else ind
    digL = length digs
    digs :: [Int]
    digs = (Digits.digits 10 prime) :: [Int]


replaceDigits :: [Int] -> [[Int]] -> [[[Digit]]]
replaceDigits oldDigits indices = L.nub candidates
  where    
    candidates = map (\indices' ->
                       map (\digit ->
                             replaceDigitsAtIndices oldDigits indices' digit 0
                           )
                       replacementDigits
                     ) indices
    replacementDigits = [0..9]
    
    
replaceDigitsAtIndices :: [Digit] -> [Int] -> Int -> Int-> [Digit]
replaceDigitsAtIndices digits [] _ _ = digits
replaceDigitsAtIndices ds@(digit:digits) is@(index:indices) replacement count
  | count == index = replacement:replaceDigitsAtIndices digits indices replacement (count+1)
  | otherwise = digit:replaceDigitsAtIndices digits is replacement (count+1)

-- How many ways can you change a list of length l without changing every original element
createIndices :: Int -> [[Int]]
createIndices l = map reverse $ createIndices' l l []

createIndices' :: Int -> Int -> [Int] -> [[Int]]
createIndices' _ 1 _ = []
createIndices' l' l [] = t ++ (concatMap (createIndices' l' (l-1)) t)
  where
    t = [[index'] | index' <- [0..(l'-1)]]
createIndices' l' l indices@(index:_) = t ++ (concatMap (createIndices' l' (l-1)) t)
  where 
    t = [index':indices | index' <- [(index+1)..(l'-1)]]


-- remove indices where the original prime does not have duplicate digits!
filterIndices :: [Digit] ->  [[Int]] -> [[Int]]
filterIndices primeDigits indices = indices'
  where
    indices' = filter (\indices' -> 
                         let firstIndexDigit = primeDigits !! (indices' !! 0)
                         in all (\index -> firstIndexDigit == (primeDigits !! index)) indices'
                         
                         ) indices
  
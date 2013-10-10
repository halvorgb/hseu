import qualified Data.Digits as Digits
  
solve = permutedMults [2..]

permutedMults :: [Int] -> Int
permutedMults (n:ns) = 
  if all (isPermutationOf n') [Digits.digits 10 (n*i) | i <- [2..6]]
  then n
  else permutedMults ns
  where
    n' = Digits.digits 10 n

isPermutationOf :: Eq a => [a] -> [a] -> Bool
isPermutationOf as bs = length as == length bs &&
                        all (\b -> elem b as) bs
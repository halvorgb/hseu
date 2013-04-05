import Data.List

getTerms :: Integer -> [Integer]
getTerms n = nub [x^y | x <-[2..n], y <- [2..n]]


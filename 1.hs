import Data.List as L

main = interact solveIt

soLveIt :: String -> String
solveIt str = show $ solve $ read str

solve :: Int -> Int
solve n = sum [x | x <- [1..n], modCheck x]


modCheck :: Int -> Bool

modCheck n
  | mod n 3 == 0 = True
  | mod n 5 == 0 = True
  | otherwise = False

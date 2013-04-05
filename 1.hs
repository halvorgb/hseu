main = interact solveIt

solveIt :: String -> String
solveIt str = show $ solve $ read str

solve :: Int -> Int
solve n = foldl (+) 0 [x | x <- [1..n], modCheck x]


modCheck :: Int -> Bool

modCheck n
  | mod n 3 == 0 = True
  | mod n 5 == 0 = True
  | otherwise = False
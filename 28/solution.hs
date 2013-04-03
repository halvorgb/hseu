main = interact solveSpiralDiagonals


solveSpiralDiagonals :: String -> String

solveSpiralDiagonals str = show (solve $ read str)

-- assumes odd n
solve :: Integer -> Integer
solve 1 = 1
solve n = sumNums n + recur
  where 
    recur = solve (n-2)

sumNums :: Integer -> Integer

sumNums n = first + second + third + fourth
  where 
    start = (n-2)^2
    first  = start  + (n-1)
    second = first  + (n-1)
    third  = second + (n-1)
    fourth = third  + (n-1)
import Data.List
import Data.Maybe

values = [1, 2, 5, 10, 20, 50, 100, 200]

solve n = solveShit (fromJust (elemIndex n values))

solveShit :: Int -> [(Int, Int)]
--                   (n, combinations)
solveShit 0 = [(1, 1)]
solveShit n = (nVal, combs):solveShit (n-1)
  where
    nVal = values !! n
    prevCombs = solveShit (n-1) -- recursion
    combs = combinations nVal prevCombs -- amount of combinations this coin can be made up of.

      
combinations :: Int -> [(Int, Int)] -> Int
combinations n ((pn, cn):t)
  | t == []    =  
  | modBy == 0 = (cn^divBy) +1 -- 
  | otherwise  = divBy + combinations modBy t
  where
    modBy = mod n pn
    divBy = div n pn


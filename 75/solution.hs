import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
--solve :: Int -> Int
--solve n = length $ filter uniqueTriangles [1..n]

{-
a + b + c = d
a² + b² = c²

a <= b < c
-}

uniqueTriangles :: Int -> Int
uniqueTriangles l = severalTriangles 0 $ concat $ zipABs as
  where
    third = div l 3
    as :: [Int]
    as = [1..(third + 1)]

    zipABs :: [Int] -> [[(Int, Int)]]
    zipABs [] = []
    zipABs (a:as) = (zip (repeat a) $ [(a+1)..(third + 2)]):zipABs as

    severalTriangles :: Int ->  [(Int, Int)] -> Int
    severalTriangles found [] = found
    severalTriangles found ((a,b):abs')
      | found == 1 &&
        cCheck = 2
      | cCheck =  severalTriangles 1 abs'

      | otherwise = severalTriangles found abs'
      where
        c = l - a - b
        cCheck = c^2 == a^2 + b^2

{-
Idè, finn antall løsninger til L_n: enten 0, 1, eller 2+.
-> For alle L_n * i, hvor i <= 15000000/n
--> Adder antall løsninger til svaret i L_(n*i).
--> ved en ny L_n, dersom antall > 1 => hopp videre umiddelbart.

-> Issue: jævlig mange redundant updates.

-}

solve :: Int -> Int
solve n = countUniqueTriangles n 1 0 Map.empty


addToMultiples :: Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
addToMultiples maxbound n val mem = foldl (\m key -> Map.adjust (+val) key m) mem keys
  where
    keys = takeWhile (<= maxbound) $ map (*n) [2..]

countUniqueTriangles :: Int -> Int -> Int -> Map.Map Int Int -> Int
countUniqueTriangles maxbound n count mem
  | n > maxbound = count
  | otherwise    = case Map.lookup n mem of
    Just c -> if c > 1
              then countUniqueTriangles maxbound n' count mem
              else countUniqueTriangles maxbound n' count' mem'
    _ -> countUniqueTriangles maxbound n' count' mem'
  where
    n' = n+1

    x = uniqueTriangles n

    (count', mem')
      | x == 1    = (count + 1, mem'')
      | x == 2    = (count, mem'')
      | otherwise = (count, mem)
      where
        mem'' = addToMultiples maxbound n x mem
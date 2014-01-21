import qualified Data.List as L

{--
  0 <= a <= 4
  1 <= b <= 5
  2 <= c <= 6
  3 <= d <= 7
  4 <= e <= 8
  5 <= f <= 9

--}

main = do
  print solve

--solve ::
-- må filtreres som faen? har både (a,b) og (b,a) i lista. svaret er halvparten?
solve = div (L.length $ L.filter cubeDigitPair cubePairs) 2
    where
      cubes :: [[Int]]
      cubes = [ (a:b:c:d:e:f:[]) | a <- [0..4], b <- [1..5], c <-[2..6], d <- [3..7], e <- [4..8], f <- [5..9], a < b && b < c && c < d && d < e && e < f]
      cubePairs :: [([Int], [Int])]
      cubePairs = concatMap (\cube -> zip (repeat cube) cubes) cubes

cubeDigitPair :: ([Int], [Int]) -> Bool
cubeDigitPair (cube1, cube2) = (not $ cube1 == cube2) &&
                               (displaysAllNumbers cube1 cube2)



displaysAllNumbers :: [Int] -> [Int] -> Bool
displaysAllNumbers cube1 cube2 =
    ((elem 0 cube1 && elem 1 cube2) || (elem 0 cube2 && elem 1 cube1)) && -- 01
    ((elem 0 cube1 && elem 4 cube2) || (elem 0 cube2 && elem 4 cube1)) && -- 04
    ((elem 2 cube1 && elem 5 cube2) || (elem 2 cube2 && elem 5 cube1)) && -- 25
    ((elem 8 cube1 && elem 1 cube2) || (elem 8 cube2 && elem 1 cube1)) && -- 81

    ((elem 0 cube1 && (elem 6 cube2 || elem 9 cube2)) || (elem 0 cube2 && (elem 6 cube1 || elem 9 cube1))) && -- 09
    ((elem 1 cube1 && (elem 6 cube2 || elem 9 cube2)) || (elem 1 cube2 && (elem 6 cube1 || elem 9 cube1))) && -- 16
    ((elem 3 cube1 && (elem 6 cube2 || elem 9 cube2)) || (elem 3 cube2 && (elem 6 cube1 || elem 9 cube1))) && -- 36

    (((elem 6 cube1 || elem 9 cube1) && elem 4 cube2) || ((elem 6 cube2 || elem 9 cube2) && elem 4 cube1)) -- 64

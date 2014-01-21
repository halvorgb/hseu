import qualified Data.List as L
import qualified Data.Set as S
{--
  1 <= a <= 6
  2 <= b <= 7
  3 <= c <= 8
  4 <= d <= 9

--}

main = do
  print solve

solve :: (Int, [Int])
solve = L.foldl (\acc@(best, mem) abcd -> let res = countConsecutiveNumbers abcd
                                          in if res > best
                                             then (res, abcd)
                                             else acc) (0,[]) abcds
    where
      abcds = [ (a:b:c:d:[]) | a <- [1..6], b <- [2..7], c <-[3..8], d <- [4..9], a < b && b < c && c < d]

data ArithmeticFunc = Plus | Minus | Div | Mult
                      deriving(Eq)

countConsecutiveNumbers :: [Int] -> Int
countConsecutiveNumbers abcd = findConsecutiveLength numbers 1
    where
      ps :: [[Int]]
      ps = L.permutations abcd

      numbers = L.foldl' generateNumbers S.empty ps

      findConsecutiveLength :: S.Set Int -> Int -> Int
      findConsecutiveLength s target
          | S.member target s = findConsecutiveLength s (target + 1)
          | otherwise         = target - 1


fs = [Plus, Minus, Div, Mult]
operators = [(x:y:z:[]) | x <- fs, y <- fs, z <- fs]


toFunc :: ArithmeticFunc -> (Double -> Double -> Double)
toFunc Plus  = (+)
toFunc Minus = (-)
toFunc Div   = (/)
toFunc Mult  = (*)
generateNumbers :: S.Set Int -> [Int] -> S.Set Int
generateNumbers s abcd = L.foldl (\s' ops -> let nums = map round $ L.filter isInt $ ambiguate ops
                                             in L.foldl' (\s'' n -> S.insert n s'') s' nums ) s operators
    where
       ambiguate :: [ArithmeticFunc] -> [Double]
       ambiguate aFs =
           [ a `x` (b `y` (c `z` d)),
             a `x` ((b `y` c) `z` d),

             (a `x` b) `y` (c `z` d),

             (a `x` (b `y` c)) `z` d,
             ((a `x` b) `y` c) `z` d
           ]
           where
             abcd' :: [Double]
             abcd'@(a:b:c:d:[]) = map fromIntegral abcd
             (x:y:z:[]) = map toFunc aFs
isInt x = x == fromInteger (round x)

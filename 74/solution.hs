module Main where
import System.Environment
import qualified Data.Digits as Digits
import qualified Data.Map as M
import qualified Data.List as L

main :: IO ()
main = do
  [n, m] <- getArgs
  putStrLn $ show $ solve (read n) (read m)

solve :: Int -> Int -> Int
solve n m = L.length $ L.filter (\k -> rotatingTerms [k] 1 n) [1..m]

rotatingTerms :: [Int] -> Int -> Int -> Bool
rotatingTerms nums rotations maxRotations
    | rotations > maxRotations = False
    | rotations == maxRotations =  membCheck
    | otherwise = if membCheck
                  then False
                  else rotatingTerms (rot:nums) rotations' maxRotations
    where
      membCheck = L.elem rot nums
      num = head nums

      rot = rotate num

      rotations' = rotations + 1


--solve n m = M.size $ L.foldl' (nRotatingTerms n) M.empty [1..m]

-- nRotatingTerms :: Int -> M.Map Int [Int] -> Int -> M.Map Int [Int]
-- nRotatingTerms n m start = nRotatingTerms' 1 n m start start

-- nRotatingTerms' :: Int -> Int -> M.Map Int Bool -> Int -> Int -> M.Map Int Bool
-- nRotatingTerms' rotations n m num start
--     | rotations > n ||
--       M.member rotation m = m
--     | rotations == n =
--         if rotation == start
--         then M.insert start True m
--         else m
--     | rotation == start = m
--     | otherwise = nRotatingTerms'  rotations' n m rotation start

--     where
--       rotation = rotate num
--       rotations' = rotations + 1


rotate :: Int -> Int
rotate start = L.foldl' (\acc dig -> acc + factorial dig) 0 digs
    where
      digs = Digits.digits 10 start

factorial :: Int -> Int
factorial n = product [n, n-1 .. 1]

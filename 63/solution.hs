import qualified Data.Digits as Digits
import qualified Data.List as L

solution = L.foldl' (\sum base ->
                      sum + (L.length $
                      L.takeWhile (test base) [1..])
                      ) 0 [1..9] -- upperBound 9 fordi digits(10 ^ n) > n for alle n


test :: Integer -> Integer -> Bool
test base pow =
  (fromIntegral pow) == nofDigits ans 0
  where
    ans :: Integer
    ans = (base ^  pow)


nofDigits :: Integer -> Int -> Int
nofDigits n mem
  | n' == 0 = mem'
  | otherwise = nofDigits n' mem'
  where
    n' = div n 10
    mem' = mem + 1
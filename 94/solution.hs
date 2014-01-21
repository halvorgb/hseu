{-
omkrets:     a + a + a + 1 = o
grunnflate: a + 1 = g
høyde:      sqrt(a² - g²/4) = h
areal:      g * h / 2

areal er integer.
a er integer.

-> høyde er integer.
 -> a² - g²/4 = n² for en/alle n.
 -> a² - (a+1)^2/4 = n²
-}

import System.Environment

-- FEIL

--main = do
--  [n] <- getArgs
--  print $ solve $ read n
--solve :: Integer -> Integer
solve n = take 15 $ filter formsIntegerArea --sum $ takeWhile (<=n) $ map (\a -> a + a + 1) candidates --undefined --f squares
    where
      formsIntegerAreaToPerimeter :: Int -> [Int]
      formsIntegerAreaToPerimeter a = []
          where
            p = a+a+a+1


      heights = map
                (\h -> let (a1, a2) = abc 3 (-2) ((-4)*fromIntegral h - 1.0)
                       in (a1,a2,h)) squares

      squares = [x^2 | x <- [1..]]



abc :: Double -> Double -> Double -> (Double, Double)
abc a b c
    | a == 0 = error "Division by zero"
    | otherwise = (x1, x2)
    where
      r = sqrt (b**2 - 4*a*c)
      x1 = ((-b) + r)/(2*a)
      x2 = (b + r)/(2*a)

isInt x = x == fromInteger (round x)

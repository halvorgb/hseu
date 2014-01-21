-- module Main where

-- import qualified Data.List as L
-- import Debug.Trace

-- main = do
--   putStrLn "halla"
-- {-
-- Du har forige "a", og n.

-- 1/(sqrt n - a) = (sqrt n + a)/(n - a^2)

-- an + (sqrt n - b)/c

-- an+1 er en funksjon av n b og c

-- -}
-- solve :: Int -> Int -> [Int]
-- solve maxN depth = map (\n ->
--                                if elem n squares
--                                then 0
--                                else let a = floor $ sqrt $ fromIntegral n
--                                         b = a
--                                         c = 1
--                                         periodic = periodicSqrt n (depth*2) b c []
-- --                                    in periodicSqrt n depth b c [a]
--                                     in periodLength periodic 1 depth

--                        ) [1..maxN]

--   where
--     squares = takeWhile (<= maxN) [x^2 | x <-[1..]]



-- periodicSqrt :: Int -> Int -> Int -> Int -> [Int] -> [Int]
-- periodicSqrt _ 0 _ _ mem = reverse mem -- base case. OBS REVERSE?

-- periodicSqrt n depth previousB previousC mem
--   | trace ("n: " ++ show n ++ ", depth: " ++ show depth ++ ", pB: " ++ show previousB ++ ", pC: " ++ show previousC ++ ", mem: " ++ show mem ++ ", an: " ++ show an ++ ", b: " ++ show b ++ ", c: " ++ show c) False = undefined
--   | an < 0 = error $ "an: " ++ show an ++ ", pB: " ++ show previousB ++ ", pC: " ++ show previousC ++ ", depth: " ++ show depth ++ " " ++ show mem
--   | otherwise = periodicSqrt n depth' b c (an:mem)
--   where
--     denominator :: Int
--     denominator = n - (previousB^2)
--     -- hvis previousC kan deles på denominator: forkort brøk
--     denominator' = div denominator $ gcd denominator previousC

--     -- a_n*denominator > previousB
--     -- => a_n > previousB/denominator
--     an = findnthA denominator' previousB
--     c = denominator'
--     b = abs $ previousB - an*denominator'

--     depth' = depth - 1

-- findnthA :: Int -> Int -> Int
-- findnthA denom b
--   | t * denom > b = t
--   | otherwise = t+1
--   where
--     t = div (2*b) denom







solve64 :: Integer -> (Integer, Integer)
solve64 n = foldl (\(odds, evens) i -> let l = periodLength (continuedFraction i) 1 (1 + 4 * (round $ sqrt $ fi i ))
                                       in if odd l
                                          then (odds+1, evens)
                                          else (odds, evens+1)
                  ) (0,0) $ filter (\i -> not $ elem i squares) [1..n]
  where
    squares = takeWhile (<=n) [x^2 | x<-[1..]]







-- | continued fraction of (sqrt s)
-- | http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
continuedFraction :: Integer -> [Integer]
continuedFraction s = a_0:contFrac s m_0 d_0 a_0
  where
    -- initial values.
    m_0 = 0
    d_0 = 1
    a_0 = floor $ sqrt $ fi s

    -- given n, find values for n+1 for m d a
    contFrac :: Integer -> Integer -> Integer -> Integer -> [Integer]
    contFrac s m_n d_n a_n = a_n1:contFrac s m_n1 d_n1 a_n1
      where
        m_n1 = d_n*a_n - m_n
        d_n1 = div (s - m_n1^2) d_n
        a_n1 = floor (((fi a_0) + (fi m_n1)) / (fi d_n1))


fi = fromIntegral







periodLength :: Eq a => Show a => [a] -> Int -> Int -> Int
periodLength [] _ _ = 0
periodLength xs len depth
  | len > depth = periodLength (tail xs) 1 depth --error $ "No repeating period for given list and depth: " ++ show (take depth xs)
  | repeatingEquality xs len n = len
  | otherwise = periodLength xs len' depth
  where
    len' = len + 1

    n = div depth len

repeatingEquality :: Eq a => [a] -> Int -> Int -> Bool
repeatingEquality xs len n = repeatingEquality' h t len n
  where
    (h, t) = splitAt len xs

repeatingEquality' :: Eq a => [a] -> [a] -> Int -> Int -> Bool
repeatingEquality' _ _ _ 0    = True
repeatingEquality' h t len n
  | h == h' = repeatingEquality' h' t' len n'
  | otherwise = False
  where
    (h', t') = splitAt len t
    n' = n - 1

-- solve64 :: Integer -> (Integer, Integer)
-- solve64 n = foldl (\(odds, evens) i -> let l = periodLength $ continuedFraction i
--                                        in if odd l
--                                           then (odds+1, evens)
--                                           else (odds, evens+1)
--                   ) (0,0) $ filter (\i -> not $ elem i squares) [1..n]
--   where
--     squares = takeWhile (<=n) [x^2 | x<-[1..]]

-- depth :: Int
-- depth = 32

-- periodLength :: [Integer] -> Int
-- periodLength ns = pLen 1 depth
--   where
--     pLen :: Int -> Int -> Int
--     pLen len maxD
--       | len > depth = error "couldnt find period"
--       | cycleOfLen ns len = len
--       | otherwise = pLen (len+1) maxD


-- cycleOfLen :: [Integer] -> Int -> Bool
-- cycleOfLen (n:ns) len = cLen 1
--   where
--     cLen :: Int  -> Bool
--     cLen offset = cycleCheck 1 offset $ splitAt len $ drop offset ns
--       -- | (toInteger offset) > (div depth 2) = False
--       -- | cycleCheck 1 offset $ splitAt offset ns = True
--       -- | otherwise = cLen (offset+1)

--     cycleCheck :: Integer -> Int -> ([Integer], [Integer]) -> Bool
--     cycleCheck count len (firstO, rest)
--       | count * (toInteger len) > (toInteger depth) = True
--       | firstO /= secondO = False
--       | otherwise = cycleCheck (count+1) len ns__
--       where
--         ns__@(secondO, _) = splitAt len rest

solve66 :: Integer -> Integer
solve66 n = snd $ foldl (\acc@(largest, index) i ->
                          let (h, _) = pellsEquation i
                          in if h > largest
                             then (h, i)
                             else acc
                        ) (1,1) $ filter (\i -> not $ elem i squares) [1..n]
  where
    squares = takeWhile (<=n) [x^2 | x<-[1..]]


-- | pells equation: a^2 + d*b^2 = 1
-- | http://en.wikipedia.org/wiki/Continued_fraction
pellsEquation :: Integer -> (Integer, Integer)
pellsEquation d = pellSolution convergents
  where
    cF = continuedFraction d

    pellSolution :: [(Integer, Integer)] -> (Integer, Integer)
    pellSolution ((h,k):convergents)
      | h^2 - d*k^2 == 1 = (h,k)
      | otherwise = pellSolution convergents

    convergents = makeConvergentList cF h_n_min_2 k_n_min_2 h_n_min_1 k_n_min_1

    -- initial values:
    -- h_n / k_n
    h_n_min_2 = 0
    k_n_min_2 = 1
    h_n_min_1 = 1
    k_n_min_1 = 0
    makeConvergentList :: [Integer] -> Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
    makeConvergentList [] _ _ _ _ = error "why isnt this list infinite."
    makeConvergentList (q:quotients) h_n_min_2 k_n_min_2 h_n_min_1 k_n_min_1 = (h_n, k_n):makeConvergentList quotients h_n_min_1 k_n_min_1 h_n k_n
      where
        h_n = q * h_n_min_1 + h_n_min_2
        k_n = q * k_n_min_1 + k_n_min_2


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
import qualified Data.List as L

maxAB = 3000

solve = L.foldl' (pentagonalDiff as) 999999999999 as
  where
    as = pentagonalList 1 maxAB []



pentagonalDiff :: [Int] -> Int -> Int -> Int
pentagonalDiff bs acc a
  | null bs = acc
  | otherwise =
    L.foldl' (\acc' b -> let d = b-a
                         in if isPentagonal d &&
                               isPentagonal (a+b)
                            then min acc' d
                            else acc'
             ) acc bs
  where
    bs' = dropWhile (<= a) bs




pentagonalList :: Int -> Int -> [Int]-> [Int]
pentagonalList i l mem
  | i > l = mem
  | otherwise = pentagonalList (i+1) l $ mem ++ [pentagonalAtI i]


isPentagonal :: Int -> Bool
isPentagonal x = 
  (fromIntegral $ round n) == n
  where
    n = (sqrt (24*fix + 1) + 1) / 6
    fix = fromIntegral x
    
pentagonalAtI :: Int -> Int
pentagonalAtI i = round $ (fi * (3.0*fi - 1.0)) / 2.0
  where
    fi = fromIntegral i
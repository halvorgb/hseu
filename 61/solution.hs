import qualified Data.Digits as Digits

fourDigitTriangles :: [Int]
fourDigitTriangles = takeWhile (<10000) $ dropWhile (<1000) [toTriangle x | x <- [1..]]

-- unødvendige iterasjoner.
--solve = filter (not . null) $ map (\triangle -> reverse $ cyclic [triangle]) fourDigitTriangles
solve = sum $ concat $ filter (not . null) $ map (\t -> cyclic' [t] [3]) fourDigitTriangles



cyclic' :: [Int] -> [Int] -> [Int]
cyclic' nums@(num:_) usedPolygonals
  | length nums == 6 = let firstDigits = take 2 $ Digits.digits 10 $ last nums
                       in if lastDigits == firstDigits
                          then nums
                          else []
  | null nextIter = []
  | otherwise = concatMap (\(poly, nums') -> 
                            concatMap (\num' ->
                                        cyclic' (num':nums) (poly:usedPolygonals)
                                      ) nums' 
                          ) nextIter
  where
    lastDigits = drop 2 $ Digits.digits 10 num
    nextIter = concatMap (\poly -> let ps = findPoly lastDigits $ intToFn poly
                                   in if null ps 
                                      then []
                                      else [(poly, ps)]
                         ) $ filter (\e -> not $ elem e usedPolygonals) [2,5,6,7,8]

intToFn :: Int -> (Int -> Bool)
intToFn i
  | i == 2 = isSquare
  | i == 5 = isPentagonal
  | i == 6 = isHexagonal
  | i == 7 = isHeptagonal
  | i == 8 = isOctagonal
  | otherwise = error "invalid input"    

findPoly :: [Int] -> (Int -> Bool) -> [Int] 
findPoly firstTwoDigits check = filter (\i -> i > 999 && check i) candidates
  where
    candidates = map (\l -> Digits.unDigits 10 $ firstTwoDigits ++ l) [[a,b] | a <- [0..9], b <- [0..9]]
    


toTriangle :: Int -> Int
toTriangle x = round $ fix*(fix+1)/2
  where
    fix = fromIntegral x
    
isTriangle :: Int -> Bool
isTriangle x = isPolygonal x fx
  where
    fx x' = (sqrt (8*x' + 1) + 1) / 2

isSquare :: Int -> Bool
isSquare x = isPolygonal x fx
  where
    fx = sqrt

isPentagonal :: Int -> Bool
isPentagonal x = isPolygonal x fx
  where
    fx x' = (sqrt (24*x' + 1) + 1) / 6

isHexagonal :: Int -> Bool
isHexagonal x = isPolygonal x fx
  where
    fx x' = (sqrt (8*x' + 1) + 1) / 4

isHeptagonal :: Int -> Bool
isHeptagonal x = isPolygonal x fx
  where
    fx x' = (sqrt (40*x' + 9) + 3) / 10


isOctagonal :: Int -> Bool
isOctagonal x = isPolygonal x fx
  where
    fx x' = (sqrt (3*x' +1) +1) / 3
    
isPolygonal ::  Int -> (Double -> Double) -> Bool
isPolygonal x fx = 
  (fromIntegral $ round n) == n
  where
    n = fx fix
    fix = fromIntegral x
    
    

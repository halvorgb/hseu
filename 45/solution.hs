solve = nextUberNumber 286

nextUberNumber :: Int -> Int
nextUberNumber n
  | isPentagonal triangle &&
    isHexagonal triangle = triangle
  | otherwise = nextUberNumber (n+1)
  where
    triangle = toTriangle n

toTriangle :: Int -> Int
toTriangle x = round $ fix*(fix+1)/2
  where
    fix = fromIntegral x



isPentagonal :: Int -> Bool
isPentagonal x = 
  (fromIntegral $ round n) == n
  where
    n = (sqrt (24*fix + 1) + 1) / 6
    fix = fromIntegral x
    


isHexagonal :: Int -> Bool
isHexagonal x = 
  (fromIntegral $ round n) == n
  where
    n = (sqrt (8*fix + 1) + 1) / 4
    fix = fromIntegral x
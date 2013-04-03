mapFuck :: [[String]] -> [[Int]]

mapFuck [] = []
mapFuck (h:t) = map read h:mapFuck t


(mapFuck (map words (lines input)))
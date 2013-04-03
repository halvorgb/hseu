main = interact findLargestSum

findLargestSum :: String -> String



getMaxSum' :: [[Int]] -> [Int]


-- BOTTOM UP
findLargestSum input = show $ head $ getMaxSum' $ reverse (map (\list -> map read list) (map words (lines input)))

getMaxSum' [root] = root
getMaxSum' (leaves:root:rest) = getMaxSum' ((zipWith (+) (pruneLeaves leaves) root):rest)

-- TOP DOWN: 
{-
findLargestSum input = show $ head $ getMaxSum' $ (map (\list -> map read list) (map words (lines input)))

getMaxSum' (root:leaves)
  | leaves == [] = pruneLeaves root
  | otherwise = zipWith (+) (getMaxSum' leaves) (pruneLeaves root)
-}

-- CALLED BY BOTH
pruneLeaves :: [Int] -> [Int]

pruneLeaves [] = []

pruneLeaves (left:right:rest) 
  | rest == [] = (max left right):pruneLeaves rest
  | otherwise = (max left right):pruneLeaves (right:rest)
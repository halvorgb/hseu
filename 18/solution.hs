main = interact findLargestSum


findLargestSum :: String -> String
-- 1. build a binary tree
findLargestSum input = show $ getMaxSum $ head tree -- getMaxSum --tree
  where
    tree = createBinaryTree $ lines input

data Tree a = Tip | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- returns lists to simplify recursion..
createBinaryTree :: [String] -> [Tree Int]
--                          breaks down a string into it's ints and creates tips.
createBinaryTree [leaves] = map (\n -> Node n Tip Tip) $ stringsToInts $ wordsWhen (==' ') leaves
createBinaryTree (roots:children) = treeHelper (stringsToInts  (wordsWhen (==' ') roots)) $ createBinaryTree children



getMaxSum :: Tree Int -> Int

getMaxSum Tip = 0
getMaxSum (Node n a b) = n + (max ca cb)
  where
    ca = getMaxSum a
    cb = getMaxSum b
    
    
-- treeHelper, an attempt to make a function that takes roots (ints) and children (trees) as parameters and then creates a list of roots (trees)
treeHelper :: [Int] -> [Tree Int] -> [Tree Int]


treeHelper [] _ = []
treeHelper (r:rs) (ca:cs) = (Node r ca cb):treeHelper rs cs
  where
    cb = head cs

    
--yep
stringsToInts :: [String] -> [Int]
stringsToInts = map read


-- splits a string into an array of strings.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
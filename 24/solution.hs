import Data.List

main = do
  putStrLn $ show answer

perm :: Eq a => [a] -> [[a]]

perm [] = [[]];
perm xs = [x:ys | x <- xs, ys <-perm (delete x xs)]

answer :: String
answer = ((sort $ perm "0123456789") !! 999999)
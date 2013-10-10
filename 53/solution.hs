import qualified Data.List as L

solve = L.foldl' (\acc n -> acc + combinationsAboveMillion n) 0 [1..100]

combinationsAboveMillion :: Integer -> Int
combinationsAboveMillion n = length $ filter (>1000000) $ map (choose n) rs 
  where
    rs = [0..n] :: [Integer]

    

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 
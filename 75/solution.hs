module Main where
import System.Environment
import qualified Data.List as L
import qualified Data.Map as M

main = do
  [n] <- getArgs
  putStrLn $ show $ solve'' $ read n

solve'' :: Integer -> Int
solve'' l = M.size $ M.filter (== 1) $ L.foldl' (generateTriangles l) M.empty ms
    where
      ms :: [Integer]
      ms = [2..(ceiling $ sqrt $ (fromIntegral l)/2)]

generateTriangles :: Integer -> M.Map Integer Int -> Integer -> M.Map Integer Int
generateTriangles l mp m = updMap l ns m mp
    where
      ns = L.filter (\n -> odd (m + n) && gcd m n == 1 ) [1..(m-1)]


updMap :: Integer -> [Integer] -> Integer -> M.Map Integer Int -> M.Map Integer Int
updMap l ns m mp = L.foldl' (\mp' n ->
                               let a = m^2 - n^2
                                   b = 2 * m * n
                                   c = m^2 + n^2
                                   p = a + b + c
                                   pms = takeWhile (<= l) $ map (*p) [1..]
                               in L.foldl' (\mp'' pm -> M.alter alterFunc pm mp'') mp' pms
                            ) mp ns
    where
      alterFunc :: Maybe Int -> Maybe Int
      alterFunc Nothing  = Just 1
      alterFunc (Just x) = Just (x + 1)

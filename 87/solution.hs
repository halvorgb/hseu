import Data.Numbers.Primes
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment

main = do
  [n] <- getArgs
  putStrLn $ show $ solve $ read n

solve :: Int -> Int
solve n = myNubFilterAndLength (<n) [fourth + cube + square | fourth <- pFourths, cube <- pCubes, square <- pSquares]
    where
      pFourths = takeWhile (<n) $ map (^4) primes
      pCubes   = takeWhile (<n) $ map (^3) primes
      pSquares = takeWhile (<n) $ map (^2) primes

myNubFilterAndLength :: (Int -> Bool) -> [Int] -> Int
myNubFilterAndLength f list = myNFL' list S.empty 0
    where
      myNFL' ::  [Int] -> S.Set Int -> Int -> Int
      myNFL' [] _ mem     = mem
      myNFL' (x:xs) s mem
          | (not $ f x) ||
            S.member x s = myNFL' xs s mem
          | otherwise    = myNFL' xs (S.insert x s) (mem+1)

test n = (length pFourths, length pCubes, length pSquares)
    where
      pFourths = reverse $ takeWhile (<n) $ map (^4) primes
      pCubes   = reverse $ takeWhile (<n) $ map (^3) primes
      pSquares = reverse $ takeWhile (<n) $ map (^2) primes

import qualified Data.List as L
import System.Environment
main = do
  [n,m] <- getArgs
  print $ solution (read n) (read m)
  

solution mn mx = L.foldl' (\acc@(md, mx) d -> if isSquare d || 
                                                 d == 97
                                              then acc 
                                              else let d' = sqrt $ fromIntegral d
                                                       x = diophantine d' 1
                                                   in if x > mx
                                                      then (d, x)
                                                      else acc) (0,0) [mn..mx]

diophantine :: Double -> Integer -> Integer
diophantine d x
  | y /= 0.0 &&
    y == (fromIntegral $ round y) = x
  | otherwise = diophantine d (x+1)
  where
    y = (sqfi (x^2 - 1))/d
    
    sqfi :: Integer -> Double
    sqfi = sqrt . fromIntegral
    
    
isSquare :: Int -> Bool
isSquare x = 
  squareRootX ^ 2 ==  x
  where
    squareRootX = round $ sqrt $ fromIntegral  x
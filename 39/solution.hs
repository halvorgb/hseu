import qualified Data.List as L

solve = L.foldl' nofTriangles (1,0) [6..1000]

-- finn alle tre tall slik at a <= b <= c && a+b+c = perimeter
-- skjekk om de danner en trekant.

nofTriangles :: (Int,Int) -> Int -> (Int,Int)
nofTriangles acc@(n,oldp) perimeter =
  if n > n'
  then acc
  else (n', perimeter)
  where
    possibleDimensions = concatMap (createDims perimeter) [1..(div perimeter 3)]
--    triangles = filter triangleDimensions possibleDimensions
    n' = length possibleDimensions
    
createDims :: Int -> Int -> [(Int, Int, Int)]
createDims p a = zip3 (repeat a) bs cs
  where
    bs = [b | b <- [a..((div p 2)-1)]]
    cs = concatMap (\b -> let c = p-a-b
                          in if c >= b &&
                                a^2+b^2 == c^2
                             then [c]
                             else []
                   ) bs
         
         
triangleDimensions :: (Int, Int, Int) -> Bool
triangleDimensions (a,b,c) =
  a^2 + b^2 == c^2




import qualified Data.List as L

solve :: Int -> [((Int, Int), (Int, Int), (Int, Int))]
solve n = triangles
    where
      v0 = (0,0)
      triangles = [(v0, (x1, y1), (x2, y2)) | x1 <- [0..n], y1 <- [0..n], x2 <- [0..n], y2 <- [0..n], formsRightTriangle v0 (x1, y1) (x2, y2)]

      formsRightTriangle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
      formsRightTriangle (x0, y0) (x1, y1) (x2, y2) =
          (not $ (x1 == 0 && y1 == 0) || (x2 == 0 && y2 == 0)) &&
          (not $ (x1 == x2 && y1 == y2)) &&
          (not $ (x2 > x1) || (y1 > y2)) &&
          a + b  == c
          where
            e01 = x1^2 + y1^2
            e02 = x2^2 + y2^2
            e12 = (x2 - x1)^2 + (y2 - y1)^2

            l :: [Int]
            l@[a, b, c] = L.sort  [e01, e02, e12]

import Data.Number.BigFloat
import Data.Char

-- feil, lag en long division algorithm hvis du gidd.

bigSqrtToSum :: Int -> Int
bigSqrtToSum a = sum $ map digitToInt $ take 100 $ drop 2 $ show $ sqrt bigFloat
    where
      bigFloat :: BigFloat (PrecPlus20 (PrecPlus20 (PrecPlus20 (PrecPlus20 Prec50))))
      bigFloat = fromIntegral a





solve = sum $ map bigSqrtToSum $ filter (not . perfectSquare) [1..100]

perfectSquare :: Int -> Bool
perfectSquare n =
    elem n squares

squares = [x^2 | x<-[1..10]]

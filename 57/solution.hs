import qualified Data.Digits as Digits
import qualified Data.Ratio as Ratio


solve = filter (largerNumerator) $ [sqrtConvergGen i | i <- [1..1000]]

largerNumerator :: Rational -> Bool
largerNumerator r = length nDigits > length dDigits
  where
    nDigits = Digits.digits 10 n
    dDigits = Digits.digits 10 d
    n = Ratio.numerator r
    d = Ratio.denominator r


sqrtConvergGen :: Int -> Rational
sqrtConvergGen i = 
  1 + sqrtConverg i
sqrtConverg :: Int -> Rational
sqrtConverg 0 = 0
sqrtConverg i =
  1/(2 + sqrtConverg (i-1))
    
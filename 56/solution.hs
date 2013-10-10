import qualified Data.Digits as Digits
import qualified Data.List as L
solve = L.foldl' max 0 $ map (sum . Digits.digits 10) ([a^b | a <- [2..100], b <- [2..100]] ::  [Integer])
import qualified Data.Digits as Digits
import qualified Data.List as L


upperBound = L.find (\(n, pow) -> (length $ (Digits.digits 10) n) > pow) powers

powers :: [(Integer, Int)]
powers = [(round (2** (fromIntegral pow)), pow) | pow <- [1..]]
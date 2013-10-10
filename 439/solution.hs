import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Numbers.Primes as Primes

num :: Integer
num = round (10^11)

sqrtNum :: Integer
sqrtNum = round $ sqrt $ fromIntegral num

-- num er på formen 10000000000, har kun to primtallsfaktorer 2 og 5.
candidates = []
  where
    ones = [1..num]
    twos = [x | x <- [2..num]]



SIKTE HØYT JA
import qualified Data.Digits as Digits
import qualified Data.List as L
import qualified Data.Numbers.Primes as Primes
digits = [1..7]
possiblePrimes = map (Digits.unDigits 10) $ L.permutations digits

ps = L.foldl' max 1 $ filter Primes.isPrime possiblePrimes
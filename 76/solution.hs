
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Digits as Digits

isSquare :: Integer -> Bool
isSquare n = Maybe.isJust $ Map.lookup n squares

squares :: Map.Map Integer Integer
squares = Map.fromList $ zip (takeWhile (<1500000) [x^2 | x<-[1..]]) $ repeat 1

solve :: Integer -> Integer -> Integer
solve n precision = foldl (\acc i ->
                            acc + (sum $ mySqrt i precision)
                          ) 0 nonSquares
  where
    nonSquares = filter (not . isSquare) [1..n]


-- http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Decimal_.28base_10.29
mySqrt :: Integer -> Integer -> [Integer]
mySqrt n precision = take (fromIntegral precision) $ decimalMethod nPaired 0 0
  where
    digs :: [Integer]
    digs = Digits.digits 10 n

    nPaired :: [Integer]
    nPaired
      | odd $ length digs = (head digs):(pairDigits $ tail digs)
      | otherwise      = pairDigits digs

    -- infinite list of pair of digits summed , 2 -> 02, 00 00 00 00 00 ...
    --                                            -> 2,  0  0  0  0  0  ...
    pairDigits :: [Integer] -> [Integer]
    pairDigits [] = repeat 0
    pairDigits (d:d':digits) = (d*10 + d'):pairDigits digits

    -- reversed infinite list.
    decimalMethod :: [Integer] -> Integer -> Integer -> [Integer]
    decimalMethod (pair:pairs) remainder p = x:decimalMethod pairs remainder' p'
      where
        c = remainder * 100 + pair
        -- x = x(20p + x) <= c, for største mulige x.
        guessX :: Integer -> Integer
        guessX x
          | x*(20*p + x) > c = (x-1)
          | otherwise = guessX (x+1)

        x = guessX 1
        y = x*(20*p + x)
        p' = p*10 + x
        remainder' = c - y

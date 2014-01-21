module Main(main) where

import qualified Data.Digits as Digits
import qualified Data.Map as Map

import System.Environment


data ArrivesAt = One | EightyNine deriving (Eq)


main = do
  [n] <- getArgs
  print $ solve $ read n

solve n = squareDigitChains [1..n] 0 Map.empty

squareDigitChains :: [Int] -> Int -> Map.Map Int ArrivesAt -> Int
squareDigitChains [] count _ = count
squareDigitChains (x:xs) count mem = squareDigitChains xs count' mem'
  where
    count'
      | a == EightyNine = count + 1
      | otherwise = count

    (a, mem') = arrivesAt x []

    arrivesAt :: Int -> [Int] -> (ArrivesAt, Map.Map Int ArrivesAt)
    arrivesAt y ys
      | y == 1    = (One, updateMap mem (y:ys) One)
      | y == 89   = (EightyNine, updateMap mem (y:ys) EightyNine)

      | otherwise = case Map.lookup y mem of
          Just a    -> (a, updateMap mem ys a)
          otherwise -> arrivesAt (nextNum y) (y:ys)


nextNum :: Int -> Int
nextNum = sum . map (^2) . Digits.digits 10

updateMap :: Map.Map Int ArrivesAt -> [Int] -> ArrivesAt -> Map.Map Int ArrivesAt
updateMap mem keys value = foldl (\m k ->
                                   Map.insert k value m) mem keys
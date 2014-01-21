module Main where
import System.Environment
import qualified Data.Digits as Digits
import qualified Data.Map as M
import qualified Data.List as L

main :: IO ()
main = do
  [n, i, i'] <- getArgs

  putStrLn $ show $ solve (read n) (read i) $ read i'

solve n i maxIndex = digitPowerSum [n..] i maxIndex




digitPowerSum :: [Integer] -> Integer -> Integer -> Integer
digitPowerSum (n:ns) index maxIndex
    | isPowerSum base base n =
        let index' = index + 1
        in if index' == maxIndex
           then n
           else digitPowerSum ns index' maxIndex
    | otherwise = digitPowerSum ns index maxIndex
    where
      base = sumDigits n




isPowerSum :: Integer -> Integer -> Integer -> Bool
-- isPowerSum _ 1 _ = False
-- isPowerSum mem base number
--     | mem' < number = isPowerSum mem' base number
--     | otherwise = mem' == number
--     where
--       mem' = mem * base

    -- power == (fromIntegral $ round power)
    -- where
    --   fib = fromIntegral base
    --   fin = fromIntegral number
    --   power = log fin / log fib
isPowerSum _ 1 _ = False
isPowerSum mem base number =
    (fromIntegral $ round lg) == lg
    where
      lg :: Double
      lg = logBase (fromIntegral base) (fromIntegral number)


sumDigits :: Integer -> Integer
sumDigits n = sum $ Digits.digits 10 n

testFunc :: Integer -> IO ()
testFunc n = do
  putStrLn $ show n
  putStrLn $ show base
  putStrLn $ show power
  putStrLn $ show $ isPowerSum base base n
    where
      base = sumDigits n

      fib = fromIntegral base
      fin = fromIntegral n
      power = log fin / log fib

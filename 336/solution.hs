import System.Environment

import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Vector.Unboxed as V


--main = do
--  [n] <- getArgs

--  putStrLn $ show $ solve $ read n


solve :: Int -> Int
solve n = length $ L.permutations ['a'..]
    where
      trainPermVectors = map (V.fromList) $
                         L.permutations $ take n ['a'..]



sortTrain :: String -> Char -> Int -> Int
sortTrain (fstCart:carts) nextCart n
    | fstCart == nextCart =
        let nextCart' = C.chr (1 + C.ord nextCart)
        in sortTrain carts nextCart' n
    | otherwise = undefined
    where
      sndCard = head

rotateList :: String -> String
rotateList = undefined

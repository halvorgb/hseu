import qualified Data.List as L
import qualified Data.Maybe as Mb
import System.Environment

import Data.String.Utils as U


main = do
  [f] <- getArgs
  fileString <- readFile f
  let lined = lines fileString
  print $ sum $ map saved lined

    where
      saved :: String -> Int
      saved rs = let l  = length rs
                     l' = length $ shortenString rs
                 in  l - l'

shortenString :: String -> String
shortenString s = U.replace "IIII"  "IV" $  -- 4
--                  U.replace "VIII" "IIX" $  -- 8
                  U.replace "VIIII" "IX" $  -- 9
                  U.replace "XXXX"  "XL" $  -- 40
--                  U.replace "LXXX" "XXC" $  -- 80
                  U.replace "LXXXX" "XC" $  -- 90
                  U.replace "CCCC"  "CD" $  -- 400
--                  U.replace "DCCC" "CCM" $  -- 800
                  U.replace "DCCCC" "CM" s  -- 900

import qualified Data.List as L

solution = L.foldl' (\acc n -> acc + n^n) 0 [1..1000]
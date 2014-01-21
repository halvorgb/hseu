import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Data.List as L

-- fra nr 31.
solve :: Int -> M.Map Int Int
solve n = M.map (+ (- 1)) $ L.foldl (\m c -> ways n m 1 c) initialMap nums
  where
    nums = [1..n]
    initialMap = M.fromList $ [(0, 1)] ++ (zip [1..n] $ repeat 0)

ways :: Int -> M.Map Int Int -> Int -> Int -> M.Map Int Int
ways maxbound mem num coin
    | num > maxbound = mem
    | otherwise =
        case prevResult of
          Just n -> let mem' = M.update (\m -> Just (m+n)) num mem
                    in ways maxbound mem' (num+1) coin
          _ -> ways maxbound mem (num+1) coin
    where
      rest = num - coin
      prevResult = M.lookup rest mem

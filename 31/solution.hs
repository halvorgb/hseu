import qualified Data.List as L
import qualified Data.Map as M
coins = [1,2,5,10,20, 50, 100]

-- finn på hvor mange måter man kan bygge opp 1 på, så 2.. så 3.


solve = L.foldl (\m c -> ways m 1 c) initialMap coins
  where
    initialMap = M.fromList $ [(0, 1)] ++ (zip [1..200] $ replicate 200 0)

ways :: M.Map Int Int -> Int -> Int -> M.Map Int Int
ways mem 201 _ = mem
ways mem num coin =
  case prevResult of
    Just n -> let mem' = M.update (\m -> Just (m+n)) num mem
              in ways mem' (num+1) coin
    _ -> ways mem (num+1) coin
  where
    rest = num - coin
    prevResult = M.lookup rest mem

import Data.List
import Control.Arrow(second)
import Data.Char (ord)

main = interact sortAndEnumerate

sortAndEnumerate :: String -> String

sortAndEnumerate input = show $ enumerate (sort $ splat input) 1



enumerate :: [String] -> Int -> Int

enumerate [] i = 0
enumerate (h:t) i = (i * (enumerate' h)) + enumerate t (i+1)

enumerate' :: String -> Int
enumerate' [] = 0
enumerate' (h:t)
  | h == '\"' = enumerate' t
  | otherwise = (ord h + 1) - ord 'A' + enumerate' t

-- break' is like break but removes the
-- delimiter from the rest string
break' d = second (drop 1) . break d

split :: String -> Maybe (String,String)
split [] = Nothing
split xs = Just . break' (==',') $ xs

splat :: String -> [String]
splat =  unfoldr $ split
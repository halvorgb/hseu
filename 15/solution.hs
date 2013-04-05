latticePaths :: Integer -> String

latticePaths input = show $ calcLatPaths input (div input 2)


calcLatPaths :: Integer -> Integer -> Integer

calcLatPaths whole half = div (fld [(half+1)..whole]) (fld [1..half])

fld :: [Integer] -> Integer
fld = foldl (*) 1
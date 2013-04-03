solveIt :: [Integer] -> Integer
solveIt [] = 0
solveIt (n:r) = 1 countLetters $ digits n + solveIt r 

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show 


countLetters :: [Integer] -> Integer
countLetters n@(h:t)
  | l == 1           = zeroToNine !! h
  | l == 2 && h == 1 = tenToNineteen !! h
  | l == 2           = twentyToNinety !! h + countLetters t
  | l == 3 && h == 0 = countLetters t
  | l == 3           = zeroToNine !! h + hundred + countLetters t
  | otherwise        = onek 
  where
    l = length n

-- TODO:NEVER
    -- Handle fucking onehundred AND two, trenger lookahead etc.


-- LENGTHS:
--zero, one, two ,three, four five,six, seven, eight, nine
zeroToNine =  [0, 3, 3, 5, 4, 4, 3, 5, 5, 4]
-- ten, eleven, twelve, thirteen, fourteen, fifteen, sixteen, seventeen, eighteen, nineteen
tenToNineteen = [3, 6, 6, 8, 8, 7, 7, 9, 8, 8]
-- (0), ^, twenty, thirty, fourty, fifty, sixty, seventy, eighty, ninety
twentyToNinety = [0, 0, 6, 6, 6, 5, 5, 7, 6, 6]
hundred = 7
and = 3
onek = length "onethousand"
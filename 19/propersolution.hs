-- main = putStrLn $ countSundays 1901 2000 06 0

{-
1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-}

year =     [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapyear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

--start, end, date of first sunday in Jan, total.
countSundays ::  Int -> Int -> Int -> Int

countSundays current end first
  | current > end = 0
  | otherwise  = (countFirstSundays days first) + (countSundays (current+1) end $ pushFirstSundayDate first days)
  where 
    days = leapCheck current
        
--               monthL,  first of Jan, count 
countFirstSundays :: [Int] -> Int -> Int

countFirstSundays [] _ = 0
countFirstSundays (days:daysPM) first
  | first == 1 = 1 + countFirstSundays daysPM (nextMonthFirst days first)
  | otherwise = countFirstSundays daysPM (nextMonthFirst days first)
                
nextMonthFirst :: Int -> Int -> Int
nextMonthFirst days first
  | fourSundays > days = fourSundays - days
  | otherwise = (fourSundays + 7) - days
  where
    fourSundays = first + (7*4)
    
    
    
    
-- called once each iteration, probably inefficient but wtevr    
-- Prev. first sunday, days, next first sundy
pushFirstSundayDate :: Int -> [Int] -> Int
pushFirstSundayDate prev daysPM
  | fiftyTwo > days = fiftyTwo - days
  | otherwise = fiftyThree - days
  where
    fiftyTwo = prev + (7*52)
    fiftyThree = prev + (7*53)
    days = foldl (+) 0 daysPM

                
leapCheck :: Int -> [Int]

leapCheck y
  | mc 400 == 0 = leapyear
  | mc 100 == 0 = year
  | mc 4 == 0   = leapyear
  | otherwise   = year
  where
    mc = mod y
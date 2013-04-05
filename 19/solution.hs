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
  | otherwise  = (countSundaysInJanuary first) + countSundays (current+1) end (pushFirstSundayDate first days)
  where 
    days = leapCheck current
    
    
        


-- Prev. first sunday, days, next first sundy
pushFirstSundayDate :: Int -> Int -> Int
pushFirstSundayDate prev days
  | fiftyTwo > days = fiftyTwo - days
  | otherwise = fiftyThree - days
  where
    fiftyTwo = prev + (7*52)
    fiftyThree = prev + (7*53)

{-
-- first sunday -> count
countSundaysInJanuary :: Int -> Int
countSundaysInJanuary first
  | first + 7 < 32 = 1 + countSundaysInJanuary (first + 7)
  | otherwise = 1
-}              
                
                
leapCheck :: Int -> [Int]

leapCheck year
  | mc 400 == 0 = 366
  | mc 100 == 0 = 365
  | mc 4 == 0   = 366
  | otherwise   = 365
  where
    mc = mod year
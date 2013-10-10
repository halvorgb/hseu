import qualified Data.Digits as Digits

lastDigit = 1000000 -- 1 mill

--listLength = 1 mill,
-- hvor mange tall er det?
-- tall * bredde
--   0 -> 9 = 10 tall * 1 = 10 digits
-- , 10 -> 99 = 90 * 2 = 180 digits,
-- , 100 -> 999 = 900 * 3 = 2700 digits,
-- , 1000 -> 9999 = 9000 * 4 = 36000 digits,
-- , 10000 -> 99999 = 90000 * 5 = 450000 digits,
-- , 100000 -> 999999 = 900000 * 6 = 5400000 digits,

digit10 = Digits.digits 10

dnl mn mx = concatMap digit10 [mn..mx]

d1 = 1
d10 = 1
d100 = dnl 10 99 !! (100-10)
d1000 = dnl 100 999 !! (1000 - 180 -10)
d10000 = dnl 1000 9999 !! (10000 - 2700 - 180 -10)
d100000 = dnl 10000 99999 !! (100000 - 36000 - 2700 - 180 -10)
d1000000 = dnl 100000 999999 !! (1000000 - 450000 - 36000 - 2700 - 180 -10)



solve = d1*
        d10*
        d100*
        d1000*
        d10000*
        d100000*
        d1000000

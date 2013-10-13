import qualified Data.Digits as Digits
import qualified Data.List as L
import qualified Control.Monad as Mon
import System.Environment
cubes :: [Integer]
cubes = dropWhile (<100) [round (x**3) | x <- [1..]]

main = do
  [n,t] <- getArgs
  putStrLn $ show $ solve (read n) (read t)

solve :: Int -> Int -> Maybe Integer
solve n t = L.find (hasNCubePerms n t) cubes


hasNCubePerms :: Int -> Int -> Integer -> Bool
hasNCubePerms terms timer cube = terms == (countCubes terms 0 $ uniquePerms digs)
  where
    digs = integerToDigits cube
                     
noLeadingZero :: [Int] -> Bool
noLeadingZero (n:_) = n /= 0    

isCube :: Integer -> Bool
isCube x =
  cubeRootX ^ 3 == fromIntegral x
  where
    cubeRootX = round $ fromIntegral x ** (1/3)
    
countCubes :: Int -> Int -> [[Int]] -> Int
countCubes _ count [] = count
countCubes goal count (digits:digitss)
  | ((head digits) /= 0) && 
    (isCube $ digitsToInteger digits) = 
      let count' = count +1
      in if count' > goal
         then 0
         else countCubes goal count' digitss
  | otherwise = countCubes goal count digitss


filterCubes :: [[Int]] -> [[Int]] -> [[Int]]
filterCubes [] mem = mem
filterCubes (digits:digitss) mem
  | (head digits) == 0 = filterCubes digitss mem
  | (isCube $ digitsToInteger digits) = filterCubes digitss (digits:mem) 
  | otherwise = filterCubes digitss mem
    
    
        
integerToDigits :: Integer -> [Int]
integerToDigits = Digits.digits 10 . fromIntegral

digitsToInteger :: [Int] -> Integer
digitsToInteger = Digits.unDigits 10 . map fromIntegral

    
-- | Find all unique permutations of a list where there might be duplicates.
uniquePerms :: (Eq a) => [a] -> [[a]]
uniquePerms = permBag . makeBag

-- | An unordered collection where duplicate values are allowed,
-- but represented with a single value and a count.
type Bag a = [(a, Int)]

makeBag :: (Eq a) => [a] -> Bag a
makeBag [] = []
makeBag (a:as) = mix a $ makeBag as
  where
    mix a []                        = [(a,1)]
    mix a (bn@(b,n):bs) | a == b    = (b,n+1):bs
                        | otherwise = bn : mix a bs

permBag :: Bag a -> [[a]]
permBag [] = [[]]
permBag bs = concatMap (\(f,cs) -> map (f:) $ permBag cs) . oneOfEach $ bs
  where
    oneOfEach [] = []
    oneOfEach (an@(a,n):bs) =
        let bs' = if n == 1 then bs else (a,n-1):bs
        in (a,bs') : mapSnd (an:) (oneOfEach bs)
    
    apSnd f (a,b) = (a, f b)
    mapSnd = map . apSnd
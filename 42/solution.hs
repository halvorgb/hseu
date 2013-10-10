import qualified Data.Text.Lazy.IO as IO 
import qualified Data.Text as Text
import qualified Data.List as L
import qualified Data.Char as Char

main = do
  f <- readFile "words.txt"
  let words = map (filter (/= '"') . Text.unpack) $ Text.splitOn (Text.pack ",") $ Text.pack f

      
  putStrLn $ show $ solve 0 words
  return ()
  
  
solve :: Int -> [String] -> Int
solve  count [] = count
solve  count (word:words) =
  solve  count' words
  where
    triangle =  (isTriangle 0 $ wordToInt word)
    count'
      | triangle = count+1
      | otherwise = count

  

wordToInt :: String -> Int
wordToInt = foldl (\acc c -> acc - 64 + Char.ord c) 0
  
            

--  BRUTEFAEN
            
isTriangle :: Int -> Int -> Bool
isTriangle i n
  | n > triangle = isTriangle (i+1) n
  | otherwise = n == triangle
  where
    triangle = round $ (fromIntegral i + 1) * (fromIntegral i) / 2.0

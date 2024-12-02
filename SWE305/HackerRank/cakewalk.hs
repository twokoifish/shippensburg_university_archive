import Data.List (sort)
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

powSum :: Int -> [Int] -> Int
powSum _ [] = 0
powSum e (x:xs) = ((2^e) * x) + powSum (e + 1) xs

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  let backwards = reverse (sort xs)
  print (powSum 0 backwards)


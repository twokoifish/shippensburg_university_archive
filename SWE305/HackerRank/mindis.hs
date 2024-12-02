readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

minimumDistances :: [Int] -> Int
minimumDistances a = 0

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  print (minimumDistances xs)


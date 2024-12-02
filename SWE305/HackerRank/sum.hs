
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine


main :: IO ()
main = do
  arr <- readInts
  print (sum arr)
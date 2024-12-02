readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main :: IO ()
main = do
  r <- readLn :: IO Int
  rs <- readInts
  p <- readLn :: IO Int
  ps <- readInts
  print r

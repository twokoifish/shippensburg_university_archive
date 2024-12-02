readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine 

main :: IO ()
main = do
  n <- readLn :: IO Int
  ranked <- readInts
  m <- readLn :: IO Int
  player <- readInts
  print "x"
   
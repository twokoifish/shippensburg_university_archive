import Control.Monad (replicateM)
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main :: IO ()
main = do
  [h,w] <- readInts
  input <- replicateM h readInts
  print (input)


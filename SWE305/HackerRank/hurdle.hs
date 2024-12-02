readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main :: IO ()
main = do
  nk <- readInts
  let n = head nk
  let k = nk!!1
  height <- readInts
  let mx = maximum height
  if mx < k then print 0
  else print (abs (mx - k))
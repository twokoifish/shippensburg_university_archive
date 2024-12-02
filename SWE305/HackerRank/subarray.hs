readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

subset :: [Int] -> Int -> Int -> Int
subset [] _ _ = 0
subset (x:xs) d m = do
  let remaining = take (m - 1) xs
  if (x + sum remaining) == d then 1 + subset xs d m
  else subset xs d m

main :: IO ()
main = do
  n <- readLn :: IO Int
  ns <- readInts
  [d, m] <- readInts
  print (subset ns d m)
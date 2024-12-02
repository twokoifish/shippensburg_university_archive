readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

picking :: [Int] -> [Int] -> Int -> Int
picking _ [] mx = mx
picking oxs (x:xs) mx = do
    let count = length (filter (\y -> y == x || y == (x - 1)) oxs)
    if count > mx then picking oxs xs count
    else picking oxs xs mx

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  print (picking xs xs 0)

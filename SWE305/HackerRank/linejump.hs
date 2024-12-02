readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

kangaroo :: Int -> Int -> Int -> Int -> String
kangaroo x1 v1 x2 v2 | (x2 - x1) * (v2 - v1) < 0 && (x2 - x1) `mod` (v2 - v1) == 0 = "YES"
                     | otherwise = "NO"

main :: IO ()
main = do
  [x1, v1, x2, v2] <- readInts
  putStrLn (kangaroo x1 v1 x2 v2)
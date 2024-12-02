readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

breakingRecords :: [Int] -> Int -> Int -> Int -> Int -> (Int, Int)
breakingRecords [] _ _ minC maxC = (minC, maxC)
breakingRecords (x:xs) 999 0 0 0 = breakingRecords xs x x 0 0
breakingRecords (x:xs) min max minC maxC
  | x < min = breakingRecords xs x max (1 + minC) maxC
  | x > max = breakingRecords xs min x minC (1 + maxC)
  | otherwise = breakingRecords xs min max minC maxC


main :: IO ()
main = do
  n <- readLn :: IO Int
  scores <- readInts
  let x = breakingRecords scores 999 0 0 0
  putStr (show (snd x))
  putStr " "
  putStrLn (show (fst x))
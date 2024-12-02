readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

cutTheSticks :: [Int] -> [Int]
cutTheSticks [] = []
cutTheSticks xs = do
  let shortest = minimum xs
  let filtered = filter (/= shortest) xs
  let newSticks = fmap (\y -> y - shortest) filtered
  length xs : cutTheSticks newSticks

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  putStr (unlines (fmap show (cutTheSticks xs)))
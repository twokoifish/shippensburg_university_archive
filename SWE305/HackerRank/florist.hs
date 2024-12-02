readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

compareTriples :: [Int] -> [Int] -> [Int]
compareTriples a b = do
  let results = [head a - head b, a!!1 - b!!1, a!!2 - b!!2]
  let bob = length (filter (> 0) results)
  let alice = length (filter  (< 0) results)
  [alice, bob]


main :: IO ()
main = do
  a <- readInts
  b <- readInts
  putStr (unlines (fmap show (compareTriples a b)))

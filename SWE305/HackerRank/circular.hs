import Control.Monad (replicateM)
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

rotateN :: Int -> [Int] -> [Int]
rotateN t xs = take (length xs) (drop t (cycle xs))

circularArrayRotation :: [Int] -> Int -> [Int] -> [Int]
circularArrayRotation a k queries = do
  let list = rotateN k a
  let mat = replicate (length queries) list
  zipWith (!!) mat queries

main :: IO ()
main = do
  [n, k, q] <- readInts
  a <- readInts
  queries <- replicateM q (readLn :: IO Int)
  let results = circularArrayRotation a k queries
  let strRes = unlines (fmap show results)
  putStr strRes


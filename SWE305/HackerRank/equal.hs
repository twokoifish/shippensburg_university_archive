import Data.List (nub, sortBy)
import Data.Function (on)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

-- Group items by occurrence.
-- [1,2,3,3] -> [(1,1),(2,1),(3,2)]
groupByOccurrence :: [Int] -> [(Int, Int)]
groupByOccurrence xs = do
   let unique = nub xs
   let mat = replicate (length unique) xs
   zipWith (\x y -> (x, length (filter (==x) y))) unique mat

countRemovals :: [(Int, Int)] -> Int -> Int
countRemovals xs x = do
  let sortedPairs = sortBy (flip compare `on` snd) xs
  let countKeep = snd (head sortedPairs)
  x - countKeep

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  let pairs = groupByOccurrence xs
  let removals = countRemovals pairs x
  print removals
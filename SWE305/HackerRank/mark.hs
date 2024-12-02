import Data.List (sort)
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

getToys :: Int -> [Int] -> Int -> [Int] -> Int
getToys _ [] _ cart = length cart - 1
getToys k (p:prices) sum cart = do
  if sum <= k then getToys k prices (sum + p) (cart ++ [p])
  else getToys k prices sum cart

main :: IO ()
main = do
  [n, k] <- readInts
  prices <- readInts
  let sorted = sort prices
  print (getToys k sorted 0 [])
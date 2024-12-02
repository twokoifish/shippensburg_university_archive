readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

isPerfectSquare :: Int -> Bool
isPerfectSquare n = do
  let possibleSq =  sqrt n
  let flr = floor possibleSq :: Int
  (possibleSq^2) == n


squares :: Int -> Int -> [Int]
squares a b = do
  let range = [a..b]
  []

main :: IO ()
main = do
  q <- readLn :: IO Int

  print ""
import Control.Monad (replicateM)

-- Get ints from IO
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

-- Output "YES" or "NO" based on input
angryProfessor :: (Int, Int, [Int]) -> String
angryProfessor input = do
  let n = first input
  let threshold = second input
  let arrivalTimes = third input
  let onTime = length (filter (<= 0) arrivalTimes)
  if onTime >= threshold then "NO"
  else "YES"

-- Get the third item in a tuple.
third :: (a, b, c) -> c
third (_, _, c) = c

-- Get the second item in a tuple.
second :: (a, b, c) -> b
second (_, b, _) = b

-- Get the first item in a tuple.
first :: (a, b, c) -> a
first (a, _, _) = a

-- Get input from the user.
getInput :: IO (Int, Int, [Int])
getInput = do
  [n, k] <- readInts
  a <- readInts
  return (n, k, a)

main :: IO ()
main = do
  t <- readLn :: IO Int
  input <- replicateM t getInput
  let results = fmap angryProfessor input
  putStr (unlines results)
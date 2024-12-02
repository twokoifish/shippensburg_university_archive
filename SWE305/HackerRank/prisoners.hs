import Control.Monad (replicateM)

readInts :: IO [Integer]
readInts = fmap (fmap read . words) getLine

saveThePrisoner :: Integer -> Integer -> Integer -> Integer
saveThePrisoner n m s = s + (m `mod` n) - 1

getInput :: IO [Integer]
getInput = do
  [n, m, s] <- readInts
  return [n, m, s]

main :: IO ()
main = do
  t <- readLn :: IO Int
  input <- replicateM t getInput
  let results = fmap (\(n:m:s:_) -> saveThePrisoner n m s) input
  putStr (unlines (fmap show results))
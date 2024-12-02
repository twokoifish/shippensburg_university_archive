import Control.Monad (replicateM)
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

minimumCost :: [Int] -> Int
minimumCost xs = do
  let b = head xs
  let w = xs!!1
  let bc = xs!!2
  let wc = xs!!3
  let z = xs!!4
  --
  if bc == wc || z >= abs (wc - bc) then (b * bc) + (w * wc)
  else if bc > (wc + z) then ((b + w) * wc) + (b * z)
  else ((b + w) * bc) + (w * z)

getInput :: IO [Int]
getInput = do
  [b, w] <- readInts
  [bc, wc, z] <- readInts
  return [b, w, bc, wc, z]

main :: IO ()
main = do
  t <- readLn :: IO Int
  input <- replicateM t getInput
  let results = fmap minimumCost input
  putStr (unlines (fmap show results))
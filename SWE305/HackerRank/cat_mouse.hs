import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

catAndMouse :: [Int] -> String
catAndMouse xyz = do
  let x = head xyz
  let y = xyz!!1
  let z = xyz!!2
  let xz = abs(x - z)
  let yz = abs(y - z)
  if xz < yz then "Cat A"
  else if yz < xz then "Cat B"
  else "Mouse C"

main :: IO ()
main = do
  q <- readLn :: IO Int
  xyzmat <- replicateM q readInts
  let results = fmap catAndMouse xyzmat
  putStr (unlines results)
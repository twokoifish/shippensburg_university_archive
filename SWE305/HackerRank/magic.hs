import Control.Monad
import Data.List ((\\), sort)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

magic :: [[Int]] -> IO ()
magic square = do
  let flatSquare = concat square
  let magicSquare = [1..9] :: [Int]
  let missing = magicSquare \\ flatSquare
  let dup = flatSquare \\ magicSquare
  print (missing)
  print (dup)

main :: IO ()
main = do
  matrix <- replicateM 3 readInts
  magic matrix
  --print (magic matrix)
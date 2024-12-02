import Data.List (elemIndex)
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

rangeFrom :: Int -> Int -> [Int]
rangeFrom x y = [x..y]

permutation :: Int -> [Int] -> Int
permutation x p =
  let
    p1 = fromJust (elemIndex x p)
    p2 = fromJust (elemIndex (p1 + 1) p)
  in (p2 + 1)
  
main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- readInts
  let rng = rangeFrom 1 x
  let xxs = replicate x xs
  let result = zipWith permutation rng xxs
  putStr (unlines (fmap show result))

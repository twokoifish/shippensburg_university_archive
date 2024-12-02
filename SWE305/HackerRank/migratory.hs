import Data.List (sort, nub, maximumBy)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

getBird :: Int -> [Int] -> Int
getBird n ns =
  let
    map = replicate n (sort ns)
    dis = nub (sort ns)
    pairs = zipWith (\x y -> (x, length (filter (== x) y))) dis map
    mx = snd (maximumBy (\(_, a) (_, b) -> compare a b) pairs)
    headWanted = dropWhile (\(_, x) -> x /= mx) pairs
  in fst (head headWanted)

main :: IO ()
main = do
  n <- readLn :: IO Int
  ns <- readInts
  print (getBird n ns)
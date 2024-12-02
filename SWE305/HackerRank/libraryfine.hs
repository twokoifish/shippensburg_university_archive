readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

libraryFine :: Int -> Int -> Int -> Int -> Int -> Int -> Int
libraryFine d1 m1 y1 d2 m2 y2 = do
  if y1 > y2 then 10000
  else if y1 == y2 && m1 > m2 then 500 * (m1 - m2)
  else if y1 == y2 && m1 == m2 && d1 > d2 then 15 * (d1 - d2)
  else 0

main :: IO ()
main = do
  [d1, m1, y1] <- readInts
  [d2, m2, y2] <- readInts
  print (libraryFine d1 m1 y1 d2 m2 y2)
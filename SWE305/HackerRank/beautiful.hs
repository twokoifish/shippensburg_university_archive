fdiv :: Int -> Int -> Float
fdiv x y = fromIntegral x / fromIntegral y

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

beautifulDay :: [Int] -> Int -> [Int]
beautifulDay [] _ = []
beautifulDay (day:days) divisor =
  let
    dayInverse = read (reverse (show day)) :: Int
    diff = abs (day - dayInverse)
    frac = diff `fdiv` divisor
  in if isInt frac then day : beautifulDay days divisor
     else beautifulDay days divisor

main :: IO ()
main = do
  ijk <- readInts
  let i = head ijk
  let j = ijk!!1
  let range = [i..j]
  let k = ijk!!2
  print (length (beautifulDay range k))

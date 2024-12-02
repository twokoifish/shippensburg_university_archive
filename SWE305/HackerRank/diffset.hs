
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

lcm' :: [Int] -> Int
lcm' []=0
lcm' [x]=x
lcm' [a,b] = lcm a b
lcm' (x:xs) = lcm x (lcm' xs)

gcd' :: [Int] -> Int
gcd' []=0
gcd' [x]=x
gcd' [a,b] = gcd a b
gcd' (x:xs) = gcd x (gcd' xs)

count :: Int -> Int -> Int -> Int -> Int
count a b c oL =
  if a <= b then do
    if b `mod` a == 0 then count (a + oL) b (c + 1) oL
    else count (a + oL) b c oL
  else c

getTotalX :: [Int] -> [Int] -> Int
getTotalX a b =
  let
    aLCM = lcm' a
    bGCD = gcd' b
    totalX = count aLCM bGCD 0 aLCM
  in totalX

main :: IO ()
main = do
  nm <- readInts
  let n = head nm
  let m = nm!!1
  a <- readInts
  b <- readInts
  print (getTotalX a b)
module BetweenTwoSets where

{--
There will be two arrays of integers. Determine all integers that satisfy the following two conditions:
  1. The elements of the first array are all factors of the integer being considered.
  2. The integer being considered is a factor of all elements of the second array.

let a = [2,6]
let b = [24,36]

There are two numbers between the arrays: 6 and 12. 
6 % 2 == 0, 6 % 6== 0, 24 % 2 == 0, and 36 % 6 == 0 for the first value. 
12 % 2 == 0, 12 % 6 == 0 and 24 % 12 == 0, 36 % 12 == 0 for the second value. Return 2.
--}

-- Allows the user to read [Int] from IO
readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

getTotalX :: [Int] -> [Int] -> Int
getTotalX a b = x
  where x = 10

-- Main function
-- Takes in two Int lists, a and b, and returns then prints the getTotalX between them.
main :: IO ()
main = do
  ab <- readInts
  let sizeA = head ab
  let sizeB = head (tail ab)
  a <- readInts
  b <- readInts
  print (getTotalX a b)

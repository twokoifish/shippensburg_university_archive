

hourToString :: Int -> String
hourToString h | h == 1    = "one"
               | h == 2    = "two"
               | h == 3    = "three"
               | h == 4    = "four"
               | h == 5    = "five"
               | h == 6    = "six"
               | h == 7    = "seven"
               | h == 8    = "eight"
               | h == 9    = "nine"
               | h == 10   = "ten"
               | h == 11   = "eleven"
               | otherwise = "twelve"






timeInWords :: Int -> Int -> String
timeInWords h m = ""

main :: IO ()
main = do
  h <- readLn :: IO Int
  m <- readLn :: IO Int
  print (timeInWords h m)
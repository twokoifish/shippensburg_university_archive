factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x - 1)

main :: IO ()
main = do
  n <- readLn :: IO Integer
  print (factorial n)
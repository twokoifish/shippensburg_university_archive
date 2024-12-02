


main :: IO ()
main = do
  n <- readLn :: IO Int -- num pages
  p <- readLn :: IO Int -- page to turn to
  print (n, p)


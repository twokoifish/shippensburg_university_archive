readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

superDigit :: String -> Int 
superDigit str = 
  let
    digitArray = map (read . (:"")) str :: [Int]
    sumA = sum digitArray
  in sumA

main :: IO ()
main = do
  [n, k] <- readInts
  let superDigitString = concat (replicate k (show n))
  print (superDigit superDigitString)
common :: String -> String -> Int
common [] _ = 0
common _ [] = 0
common (a:as) (b:bs) = do
  if a == b then 1 + common as bs
  else 0

appendAndDelete :: String -> String -> Int -> IO ()
appendAndDelete s t k
   | (totalLength - (2 * commonLength)) > k                  = putStrLn "No"
   | (totalLength - (2 * commonLength)) `mod` 2 == k `mod` 2 = putStrLn "Yes"
   | (totalLength - k) < 0            = putStrLn "Yes"
   | otherwise                        = putStrLn "No"
   where commonLength = common s t
         totalLength = length s + length t

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  k <- readLn :: IO Int
  appendAndDelete s t k

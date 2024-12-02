countingValleys :: String -> Int -> Int -> Int
countingValleys [] valleys level = valleys
countingValleys (x:xs) valleys level =
  let
    newLevel = if x == 'U' then level + 1 else level - 1
  in if newLevel == 0 && x == 'U' then countingValleys xs (valleys + 1) newLevel
     else countingValleys xs valleys newLevel

main :: IO ()
main = do
  steps <- readLn :: IO Int
  path <- getLine
  print (countingValleys path 0 0)
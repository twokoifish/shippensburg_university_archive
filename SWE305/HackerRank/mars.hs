marsExploration :: String -> Int
marsExploration [] = 0
marsExploration message = do
  let msg = take 3 message
  if msg == "SOS" then 0 + marsExploration (drop 3 message)
  else 1 + marsExploration (drop 3 message)

main :: IO ()
main = do
  message <- getLine
  print (marsExploration message)
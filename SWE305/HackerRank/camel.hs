import Data.Char (isUpper)

camelCase :: String -> Int
camelCase [] = 1
camelCase (s:sx) = if isUpper s then 1 + camelCase sx else 0 + camelCase sx

main :: IO ()
main = do
  sx <- getLine
  print (camelCase sx)

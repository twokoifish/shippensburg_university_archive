import Data.Char (ord)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

getIndex :: Char -> Int
getIndex c = ord c - 97

designerPDFViewer :: [Int] -> String -> Int
designerPDFViewer h word =
  let
    asInts = fmap getIndex word
    maxChar = maximum (fmap (h !!) asInts)
  in (length word * maxChar)

main :: IO ()
main = do
  h <- readInts
  word <- getLine
  print (designerPDFViewer h word)
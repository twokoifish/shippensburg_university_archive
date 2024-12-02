import Control.Monad (replicateM)

utopianTree :: Int -> Int -> Int -> Int
utopianTree n h eN =
  let
    change
      | n == 0 = h
      | odd n = h * 2
      | otherwise = h + 1
  in if n == eN then change
     else utopianTree (n + 1) change eN

main :: IO ()
main = do
  t <- readLn :: IO Int
  ns <- replicateM t (readLn :: IO Int)
  let results = fmap (utopianTree 0 1) ns
  let charRes = fmap show results
  putStr (unlines charRes)
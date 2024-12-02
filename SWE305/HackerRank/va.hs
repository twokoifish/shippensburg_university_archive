viralAdvertising :: Int -> Int -> Int -> Int
viralAdvertising desiredDay currentDay basePeople =
  let
    likedAd = basePeople `div` 2 -- amount of people enjoyed the ad
  in if desiredDay == currentDay then likedAd
     else likedAd + viralAdvertising desiredDay (currentDay + 1) (likedAd * 3)

main :: IO ()
main = do
  n <- readLn :: IO Int
  print (viralAdvertising n 1 5)

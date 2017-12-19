import Day9

main = do
  string <- getContents
  print $ totalGarbage $ getChars string

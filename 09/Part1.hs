import Day9

main = do
  string <- getContents
  print $ countGroups $ getChars string

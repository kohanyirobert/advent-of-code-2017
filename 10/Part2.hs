import Day10

main = do
  string <- getContents
  print $ knotHash Hexadecimal $ string

import Day14

main = do
  string <- getContents
  let grid = getGrid string
  print $ countUsed grid

import Day14

main = do
  string <- getContents
  let grid = getGrid string
  print $ countState Used grid

import Day14

main = do
  string <- getContents
  let grid = updateRegions . getGrid $ string
  print . countRegions $ grid

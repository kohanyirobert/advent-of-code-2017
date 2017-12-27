import Day15

main = do
  string <- getContents
  let firstPair = getFirstPair string
  let pairs = getPairs 40000000 firstPair
  let matches = filter isMatch pairs
  print $ length matches

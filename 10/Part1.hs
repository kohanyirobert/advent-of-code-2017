import Day10

main = do
  string <- getContents
  let sizes = getSizes string
  let (first : second : rest) = knotHash 1 sizes [0..255]
  print $ first * second

import Day10

main = do
  string <- getContents
  let sizes = getSizes string
  let (first : second : rest) = knotHash sizes [0..255]
  print $ first * second

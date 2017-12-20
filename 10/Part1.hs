import Day10

main = do
  string <- getContents
  let commaToSpace = \c -> if c == ',' then ' ' else c
  let sizes = getSizes string commaToSpace
  let (first : second : rest) = knotHash sizes [0..255]
  print $ first * second

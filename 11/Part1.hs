import Day11

main = do
  string <- getContents
  let directions = getDirections string
  let origin = (0, 0, 0)
  let target = hexTransform origin directions
  print $ hexDistance origin target

import Day11

main = do
  string <- getContents
  let directions = getDirections string
  let origin = (0, 0, 0)
  let targets = hexTransform' origin directions
  let distances = map (\t -> hexDistance origin t) targets
  print $ maximum distances

import Day21

main = do
  string <- getContents
  let rules = getRules string
      pattern = (iterate (enhancePattern rules) startPattern) !! 5
  print . countCharInPattern '#' $ pattern

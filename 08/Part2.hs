import Day8

main = do
  string <- getContents
  let instructions = getInstructions string
  let (_, top) = followInstructions instructions
  print top

import Day8

main = do
  string <- getContents
  let instructions = getInstructions string
  let (state, _) = followInstructions instructions
  print $ findTopValue state

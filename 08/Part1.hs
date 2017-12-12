import Day8

main = do
  string <- getContents
  let instructions = getInstructions string
  let state = followInstructions instructions
  let Just (_, value) = findLargestRegister state
  print value

import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState instructions
      (State {backup = (frequency : [])}) = runInstructions ((/=) [] . backup) state
  print frequency

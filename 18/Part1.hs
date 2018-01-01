import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState instructions
      (State {received = (value : [])}) = runInstructions ((/=) [] . received) state
  print value

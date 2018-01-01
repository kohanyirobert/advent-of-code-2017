import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState 0 instructions
      (State {received = (value : [])}) = runInstructions ((/=) [] . received) state
  print value

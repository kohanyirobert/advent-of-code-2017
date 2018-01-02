import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState 0 Solo instructions
      (State {received = (rcv : [])}) = runSolo noMoreInstructions state
  print rcv

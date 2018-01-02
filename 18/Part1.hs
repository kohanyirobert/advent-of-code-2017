import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState 0 Solo instructions
      (State {received = (rcv : [])}) = runSolo hasReceived state
  print rcv

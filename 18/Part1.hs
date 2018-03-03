import Day18

isDone :: [State] -> Bool
isDone (s:[]) = received s /= []

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState False 0 instructions
      (State {received = (rcv:[])}:[]) = runInstructions isDone [state]
  print rcv

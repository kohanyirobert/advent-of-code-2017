import Day18

isDone :: [State] -> Bool
isDone states = all ((Waiting ==) . status) states

main = do
  string <- getContents
  let instructions = getInstructions string
      states = [makeState False 0 instructions, makeState False 1 instructions]
      states' = runInstructions isDone states
      (Just (State {sent = snd})) = findState 1 states'
  print . length $ snd

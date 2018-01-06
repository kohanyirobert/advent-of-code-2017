import Day18

main = do
  string <- getContents
  let instructions = getInstructions string
      states = [makeState 0 instructions, makeState 1 instructions]
      states' = runInstructions states
      (Just (State {sent = snd})) = findState 1 states'
  print . length $ snd

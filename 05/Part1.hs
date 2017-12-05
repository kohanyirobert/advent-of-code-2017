import Day5

main = do
  string <- getContents
  let jumps = getJumps string
  let step = followJumps succ jumps
  print step

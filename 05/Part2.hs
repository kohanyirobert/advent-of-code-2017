import Day5

main = do
  string <- getContents
  let jumps = getJumps string
  let func =
        \x ->
          x +
          if x >= 3
            then -1
            else 1
  let step = followJumps func jumps
  print step

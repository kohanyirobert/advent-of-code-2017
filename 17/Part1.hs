import Day17

main = do
  string <- getContents
  let stepSize = read string
  let spinlock = newSpinlock stepSize
  let (Spinlock _ position _ buffer) = iterate stepSpinlock spinlock !! 2017
  print $ buffer !! (position + 1)

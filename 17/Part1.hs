import Day17

main = do
  string <- getContents
  let stepSize = read string
  let spinlock = makeSpinlock (makeCircularBuffer) stepSize
  let (Spinlock _ position _ (CircularBuffer _ values)) =
        stepSpinlock 2017 spinlock
  print $ values !! (position + 1)

import Day17

main = do
  string <- getContents
  let stepSize = read string
  let spinlock = makeSpinlock (makeValueBuffer 1) stepSize
  let (Spinlock _ _ _ (ValueBuffer _ _ (Just value))) =
        stepSpinlock (50000000 - 1) spinlock
  print $ value

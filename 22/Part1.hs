import Day22

main = do
  string <- getContents
  let cluster = getCluster string
      state = State cluster startCarrier
      state' = bursts 10000 defaultBehavior state
  print . infects . carrier $ state'

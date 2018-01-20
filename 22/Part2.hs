import Day22

main = do
  string <- getContents
  let cluster = getCluster string
      state = State cluster startCarrier
      state' = bursts 10000000 evolvedBehavior state
  print . infects . carrier $ state'

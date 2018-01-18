import Day22

main = do
  string <- getContents
  let cluster = getCluster string
      state = State cluster startCarrier
      state' = (bursts state) !! 10000
  print . infects . carrier $ state'

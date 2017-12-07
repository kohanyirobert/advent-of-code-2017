import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let (nodes, _) = partitionTowers towers
  let (Detail name _, _) = findRoot nodes
  print name

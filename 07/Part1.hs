import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let ((Detail name _, _), _, _) = partitionTowers towers
  print name

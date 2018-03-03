import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let (nodes, _) = partitionTowers towers
  let Tower (Detail name _ _) _ = findRoot nodes
  putStrLn name

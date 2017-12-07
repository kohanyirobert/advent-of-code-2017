import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let (Detail name _, _) = findRoot $ findPossibleRoots towers
  print name

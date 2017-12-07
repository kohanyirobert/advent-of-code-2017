import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let (name, _, _) = findRoot $ findPossibleRoots towers
  print name

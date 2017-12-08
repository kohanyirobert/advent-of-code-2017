import Day7

main = do
  string <- getContents
  let towers = getTowers string
  let tree = buildTree towers
  let Just unbalanced = findUnbalanced tree
  let balancedWeight = getBalancedWeight unbalanced
  print balancedWeight

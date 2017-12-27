import Day15

main = do
  string <- getContents
  let (firstA, firstB) = getFirstPair string
  let generatorA = generator factorA firstA
  let generatorB = generator factorB firstB
  let pairs = getPairs 40000000 generatorA generatorB
  let matches = filter isMatch pairs
  print $ length matches

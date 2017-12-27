import Day15

main = do
  string <- getContents
  let (firstA, firstB) = getFirstPair string
  let generatorA = filter (\a -> a `mod` 4 == 0) $ generator factorA firstA
  let generatorB = filter (\b -> b `mod` 8 == 0) $ generator factorB firstB
  let pairs = getPairs 5000000 generatorA generatorB
  let matches = filter isMatch pairs
  print $ length matches

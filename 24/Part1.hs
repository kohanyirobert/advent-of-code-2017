import Day24

main = do
  string <- getContents
  let magnets = getMagnets string
      trees = buildTrees magnets
      bridges = concatMap findBridges trees
      strengths = map bridgeToStrength bridges
  print $ maximum strengths

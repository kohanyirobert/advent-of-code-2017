import Day12

import Data.Maybe (fromJust)

main = do
  string <- getContents
  let programs = getPrograms string
  let nodes = buildAllGraphs programs
  let ids = collectAllIds nodes
  print $ length ids

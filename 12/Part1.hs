import Day12

import Data.Maybe (fromJust)

main = do
  string <- getContents
  let programs = getPrograms string
  let targetProgram = fromJust $ findProgram 0 programs
  let node = buildGraph targetProgram programs
  let ids = collectIds node
  print $ length ids

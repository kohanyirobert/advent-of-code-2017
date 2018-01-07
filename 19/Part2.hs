import Day19

main = do
  string <- getContents
  let diagram = getDiagram string
      start = findStart diagram
      packet = makePacket start
      (Packet _ _ _ steps) = followPath diagram packet
  print steps

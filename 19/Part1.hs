import Day19

main = do
  string <- getContents
  let diagram = getDiagram string
      start = findStart diagram
      packet = makePacket start
      (Packet _ _ chars) = followPath diagram packet
  print chars

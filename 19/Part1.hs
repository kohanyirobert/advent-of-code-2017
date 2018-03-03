import Day19

main = do
  string <- getContents
  let diagram = getDiagram string
      start = findStart diagram
      packet = makePacket start
      (Packet _ _ chars _) = followPath diagram packet
  putStrLn chars

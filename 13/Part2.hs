import Day13

main = do
  string <- getContents
  let layers = getLayers string
  let delay = slipPacket layers
  print delay

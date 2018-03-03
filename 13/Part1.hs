import Day13

main = do
  string <- getContents
  let layers = getLayers string
  let triggeredLayers = ridePacket layers
  let totalSeverity =
        sum $ map (\(Layer _ _ severity _) -> severity) triggeredLayers
  print totalSeverity

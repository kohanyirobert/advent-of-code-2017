import Day25

main = do
  string <- getContents
  print . checksum . run . getBlueprint $ string

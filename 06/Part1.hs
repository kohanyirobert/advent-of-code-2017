import Day6

main = do
  string <- getContents
  let banks = getBanks string
  let cycle = redistributeBanks'' banks
  print cycle

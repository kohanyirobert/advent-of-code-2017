import Day6

main = do
  string <- getContents
  let banks = getBanks string
  let (_, cycle) = countRedistributionCycles banks
  print cycle

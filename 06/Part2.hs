import Day6

main = do
  string <- getContents
  let banks = getBanks string
  let (nextBanks,_) = countRedistributionCycles banks
  let (_,secondCycle) = countRedistributionCycles nextBanks
  print secondCycle

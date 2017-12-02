import Day2

minMaxDiff :: [Int] -> Int
minMaxDiff row = abs (minimum row - maximum row)

main = do
  string <- getContents
  let table = getTable string
  print (getChecksum table minMaxDiff)

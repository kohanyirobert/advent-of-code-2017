import Debug.Trace

import Day2

main = do
  string <- getContents
  let table = getTable string
  print (getChecksum table)

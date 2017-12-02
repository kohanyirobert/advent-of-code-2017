import Day2

evenQuotient :: [Int] -> Int
evenQuotient row = head [x `div` y | x <- row, y <- row, x /= y, x `mod` y == 0]

main = do
  string <- getContents
  let table = getTable string
  print (getChecksum table evenQuotient)

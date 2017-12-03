import Day3

main = do
  string <- getLine
  let number = read string :: Int
  print (calcDist number)

import Day1

main = do
  string <- getLine
  let number = read string
  let digits = toDigits number
  let size = length digits
  let halfSize = size `div` 2
  print $ sumEqualDigitsN digits halfSize 0

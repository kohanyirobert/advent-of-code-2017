import Day1

main = do
  string <- getLine
  let number = read string
  let digits = toDigits number
  print $ sumEqualDigitsN digits 1 0

import Day1

main = do
  s <- getLine
  let d = read s :: Integer
  let ds = toDigits d
  print (sumEqualDigitsN ds 1 0)

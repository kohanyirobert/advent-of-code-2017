import Day1

main = do
  s <- getLine
  let d = read s :: Integer
  let ds = toDigits d
  let l = length ds
  let hl = l `div` 2
  print (sumEqualDigitsN ds hl 0)

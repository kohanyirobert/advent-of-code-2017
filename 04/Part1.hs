import Day4

isValidPassphrase:: (Eq a) => [a] -> Bool
isValidPassphrase [] = True
isValidPassphrase (x:xs) = not (x `elem` xs) && isValidPassphrase xs

main = do
  string <- getContents
  let passphrases = getPassphrases string
  let count = countValidPassphrases passphrases isValidPassphrase
  print count

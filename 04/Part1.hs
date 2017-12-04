import Day4

main = do
  string <- getContents
  let passphrases = getPassphrases string
  let count = foldl (\b a -> b + if isValidPassphrase a then 1 else 0) 0 passphrases
  print count

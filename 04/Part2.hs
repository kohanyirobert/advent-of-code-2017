import Day4

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort [a | a <- xs, a <= x] ++ [x] ++ sort [a | a <- xs, x < a]

isAnagram :: (Ord a) => [a] -> [a] -> Bool
isAnagram a b = sort a == sort b

isValidPassphrase [] = True
isValidPassphrase (x:xs) = all (\a -> not (isAnagram x a)) xs && isValidPassphrase xs

main = do
  string <- getContents
  let passphrases = getPassphrases string
  let count = countValidPassphrases passphrases isValidPassphrase
  print count

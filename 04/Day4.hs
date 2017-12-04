module Day4 where

getPassphrases :: String -> [[String]]
getPassphrases string = map words $ lines string

isValidPassphrase :: [String] -> Bool
isValidPassphrase [] = True
isValidPassphrase (x:xs) = not (x `elem` xs) && isValidPassphrase xs

module Day4 where

getPassphrases :: String -> [[String]]
getPassphrases string = map words $ lines string

countValidPassphrases :: (Eq a) => [[a]] -> ([a] -> Bool) -> Int
countValidPassphrases passphrases validator =
  let reducer =
        \b a ->
          b +
          if validator a
            then 1
            else 0
  in foldl reducer 0 passphrases

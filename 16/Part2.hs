import Day16

main = do
  string <- getContents
  let moves = getDanceMoves string
  let programs = ['a'..'p']
  let variations = findProgramVariations programs moves
  let size = length variations
  let remainder = 1000000000 `mod` size
  print $ variations !! remainder

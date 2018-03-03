import Day16

main = do
  string <- getContents
  let moves = getDanceMoves string
  let programs = ['a' .. 'p']
  putStrLn $ followDanceMoves programs moves

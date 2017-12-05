import Day5

followJumps' :: Step -> Position -> Jumps -> Step
followJumps' step position jumps
  | newPosition < 0 || newPosition > size - 1 = nextStep
  | otherwise = followJumps' nextStep newPosition newJumps
  where size = length jumps
        offset = jumps !! position
        newOffset = offset + 1
        nextStep = step + 1
        newPosition = position + offset
        newJumps = let js = splitAt position jumps
                       hs = fst js
                       ts = tail $ snd js
                   in hs ++ [newOffset] ++ ts

followJumps :: Jumps -> Step
followJumps jumps = followJumps' 0 0 jumps

main = do
  string <- getContents
  let jumps = getJumps string
  let step = followJumps jumps
  print step

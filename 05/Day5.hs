module Day5 where

import Data.Sequence

type Step = Int

type Position = Int

type Jump = Int

type Jumps = Seq Jump

type Offset = Int

type OffsetFunction = (Offset -> Offset)

getJumps :: String -> Jumps
getJumps string = fromList $ map read $ lines string

followJumps' :: OffsetFunction -> Step -> Position -> Jumps -> Step
followJumps' func step position jumps
  | newPosition < 0 || newPosition > size - 1 = nextStep
  | otherwise = followJumps' func nextStep newPosition newJumps
  where
    size = Data.Sequence.length jumps
    offset = index jumps position
    newOffset = func offset
    nextStep = step + 1
    newPosition = position + offset
    newJumps = update position newOffset jumps

followJumps :: OffsetFunction -> Jumps -> Step
followJumps func jumps = followJumps' func 0 0 jumps

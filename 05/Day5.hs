module Day5 where

type Step = Int
type Position = Int
type Jump = Int
type Jumps = [Jump]

getJumps :: String -> Jumps
getJumps string = map read $ lines string

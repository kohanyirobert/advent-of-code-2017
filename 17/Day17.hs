module Day17 where

type StepSize = Int
type Position = Int
type Buffer = [Int]

data Spinlock = Spinlock StepSize Position Buffer
  deriving Show

newSpinlock :: StepSize -> Spinlock
newSpinlock stepSize = Spinlock stepSize 0 [0]

stepSpinlock :: Spinlock -> Spinlock
stepSpinlock (Spinlock stepSize position buffer) = Spinlock stepSize nextPosition newBuffer
  where nextValue = (buffer !! position) + 1
        bufferSize = length buffer
        nextPosition = ((position + stepSize) `mod` bufferSize) + 1
        (bufferHead, bufferTail) = splitAt nextPosition buffer
        newBuffer = bufferHead ++ [nextValue] ++ bufferTail

module Day17 where

type Count = Int

type StepSize = Int

type Value = Int

type Position = Int

type Size = Int

data Buffer
  = CircularBuffer Size
                   [Value]
  | ValueBuffer Size
                Position
                (Maybe Value)
  deriving (Show)

{-
  `stepSpinlock' doesn't access the updated value of `value' and `buffer' thus
  thunks keep accumulating, making them strict here forces evaluation instead of
  thunk creation avoiding space leak.
-}
data Spinlock =
  Spinlock StepSize
           Position
           !Value
           !Buffer
  deriving (Show)

makeSpinlock :: Buffer -> StepSize -> Spinlock
makeSpinlock buffer stepSize = Spinlock stepSize 0 0 buffer

makeCircularBuffer :: Buffer
makeCircularBuffer = CircularBuffer 1 [0]

makeValueBuffer :: Position -> Buffer
makeValueBuffer position
  | position <= 1 = ValueBuffer 1 position Nothing
  | otherwise = error "doesn't work with positions greater than 1"

updateBuffer :: Position -> Value -> Buffer -> Buffer
updateBuffer position value (CircularBuffer size values) = newBuffer
  where
    (valuesHead, valuesTail) = splitAt position values
    newValues = valuesHead ++ [value] ++ valuesTail
    nextSize = size + 1
    newBuffer = CircularBuffer nextSize newValues
updateBuffer position value (ValueBuffer size trackedPosition trackedValue)
  | position == trackedPosition =
    ValueBuffer nextSize trackedPosition (Just value)
  | otherwise = ValueBuffer nextSize trackedPosition trackedValue
  where
    nextSize = size + 1

bufferSize :: Buffer -> Int
bufferSize (CircularBuffer size _) = size
bufferSize (ValueBuffer size _ _) = size

stepSpinlock :: Count -> Spinlock -> Spinlock
stepSpinlock count spinlock@(Spinlock stepSize position value buffer)
  | count == 0 = spinlock
  | otherwise = stepSpinlock nextCount newSpinlock
  where
    size = bufferSize buffer
    nextCount = count - 1
    nextPosition = ((position + stepSize) `mod` size) + 1
    nextValue = value + 1
    newBuffer = updateBuffer nextPosition nextValue buffer
    newSpinlock = Spinlock stepSize nextPosition nextValue newBuffer

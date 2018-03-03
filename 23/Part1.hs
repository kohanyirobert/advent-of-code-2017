import Day18

import qualified Data.Map.Strict as Map

import Data.Maybe (fromJust)

isDone :: [State] -> Bool
isDone (s:[]) = pointer s == Nothing

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState False 0 instructions
      (state':[]) = runInstructions isDone [state]
  print . fromJust . Map.lookup "mul" . statistics $ state'

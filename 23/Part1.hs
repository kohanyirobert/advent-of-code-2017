import Day18

import qualified Data.Map.Strict as Map

import Data.Maybe (fromJust)

main = do
  string <- getContents
  let instructions = getInstructions string
      state = makeState 0 instructions
      (state' : []) = runInstructions [state]
  print . fromJust . Map.lookup "mul" . statistics $ state'

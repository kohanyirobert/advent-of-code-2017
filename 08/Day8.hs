module Day8 where

import qualified Data.Map.Strict as Map

import Data.Char (isAlphaNum, isSpace, isControl)
import Data.Maybe (isNothing, fromJust)

type Register = String
type State = Map.Map Register Int

data Operator = Operator String (Int -> Int -> Int)
data Comparison = Comparison String (Int -> Int -> Bool)

data Instruction = Instruction Register Operator Int Register Comparison Int

instance Show Instruction
  where show (Instruction r1 (Operator op _) n1 r2 (Comparison cmp _) n2) =
          r1 ++ " " ++ op ++ " " ++ (show n1) ++ " if " ++ r2 ++ " " ++ cmp ++ " " ++ (show n2)

-- Map.insertWith passes new and old value to it's function in this order,
-- making subtracting negative numbers to blow up, e.g.
--
--   10 - (-3) would be 13, but it ends up being (-3) - 10 which is -13
toInstruction :: [String] -> Instruction
toInstruction (r1 : op : n1 : _ : r2 : cmp : n2 : []) = instruction
  where operator = if op == "inc"
                   then Operator op (\new old -> old + new)
                   else Operator op (\new old -> old - new)
        comparison = if cmp == "=="
                     then Comparison cmp (==)
                     else if cmp == "!="
                          then Comparison cmp (/=)
                          else if cmp == "<"
                               then Comparison cmp (<)
                               else if cmp == "<="
                                    then Comparison cmp (<=)
                                    else if cmp == ">"
                                         then Comparison cmp (>)
                                         else Comparison cmp (>=)
        number1 = read n1
        number2 = read n2
        instruction = Instruction r1 operator number1 r2 comparison number2

getInstructions :: String -> [Instruction]
getInstructions string = map toInstruction $ map words $ lines string

followInstructions' :: [Instruction] -> State -> State
followInstructions' [] state = state
followInstructions' (instruction@(Instruction r1 op n1 r2 cmp n2) : instructions) state
  | comparator (Map.findWithDefault 0 r2 state) n2 = followInstructions' instructions newState
  | otherwise = followInstructions' instructions state
  where (Operator _ operator) = op
        (Comparison _ comparator) = cmp
        newState = Map.insertWith operator r1 n1 state

-- Need to initialize every key to 0, because Map.insertWith works incorrectly otherwise, e.g.
--
--   given `reg1 dec -123` if reg2 == 0' (when the condition holds) -123 will be inserted
--   into the map as-is instead of doing `0 - -123' which is 123 instead
followInstructions :: [Instruction] -> State
followInstructions instructions = followInstructions' instructions state
  where state = foldl (\m (Instruction r1 _ _ _ _ _) -> Map.insert r1 0 m) Map.empty instructions

findLargestRegister :: State -> Maybe (Register, Int)
findLargestRegister state = Map.foldlWithKey findMaximumValue Nothing state
  where findMaximumValue = \r k v -> if isNothing r
                                     then Just (k,v)
                                     else if snd (fromJust r) < v
                                          then Just (k,v)
                                          else r

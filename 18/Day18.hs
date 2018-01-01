module Day18 where

import qualified Data.Map.Strict as Map

import Data.Char (isAlpha)

type ProgId = Int
type Register = String
type Operator = String
type Queue = [Int]
type Processor = Map.Map String Int
type Pointer = Int
type Offset = Int
type Instruction = State -> State

data State = State { progId :: ProgId
                   , pointer :: Maybe Pointer
                   , instructions :: [Instruction]
                   , processor :: Processor
                   , sent :: Queue
                   , received :: Queue
                   }

instance Show State where
  show (State {pointer = p, sent = snd, processor = proc, received = rcv}) =
    "State {pointer = " ++ show p ++
    ", sent = " ++ show snd ++
    ", processor = " ++ show proc ++ 
    ", received = " ++ show rcv ++ 
    "}"

isRegister :: String -> Bool
isRegister = all isAlpha

coerceToValue :: String -> Processor -> Int
coerceToValue s p = if isRegister s then Map.findWithDefault 0 s p else read s

movePointer :: Offset -> State -> State
movePointer offset state@(State {pointer = (Just p), instructions = is}) =
  let p' = p + offset
      size = length is
  in if 0 <= p' && p' <= size - 1
     then state {pointer = Just p'}
     else state {pointer = Nothing}

sndRegister :: Register -> State -> State
sndRegister r state@(State {sent = snd, processor = proc}) =
  let snd' = (Map.findWithDefault 0 r proc) : snd
  in state {sent = snd'}

setRegister :: Register -> String -> State -> State
setRegister r s state@(State {processor = proc}) =
  let proc' = Map.insert r (coerceToValue s proc) proc
  in state {processor= proc'}

addRegister :: Register -> String -> State -> State
addRegister r s state@(State {processor = proc}) =
  let proc' = Map.adjust (\a -> a + coerceToValue s proc) r proc
  in state {processor= proc'}

mulRegister :: Register -> String -> State -> State
mulRegister r s state@(State {processor = proc}) =
  let proc' = Map.adjust (\a -> a * coerceToValue s proc) r proc
  in state {processor= proc'}

modRegister :: Register -> String -> State -> State
modRegister r s state@(State {processor = proc}) =
  let proc' = Map.adjust (\a -> a `mod` coerceToValue s proc) r proc
  in state {processor= proc'}

rcvRegister :: Register -> State -> State
rcvRegister r state@(State {sent = snd, processor = proc, received = rcv}) =
  if coerceToValue r proc == 0
  then state
  else state {sent = drop 1 snd, received = head snd : rcv}

jgzPointer :: String -> String -> State -> State
jgzPointer x y state@(State {processor = proc}) =
  let offset = if coerceToValue x proc == 0
               then 1
               else coerceToValue y proc
  in movePointer offset state

stringToInstruction :: [String] -> Instruction
stringToInstruction (o : x : []) = case o of
  "snd" -> movePointer 1 . sndRegister x
  "rcv" -> movePointer 1 . rcvRegister x

stringToInstruction (o : x : y : []) = case o of
  "set" -> movePointer 1 . setRegister x y
  "add" -> movePointer 1 . addRegister x y
  "mul" -> movePointer 1 . mulRegister x y
  "mod" -> movePointer 1 . modRegister x y
  "rcv" -> movePointer 1
  "jgz" -> jgzPointer x y

getInstructions :: String -> [Instruction]
getInstructions string = map (stringToInstruction . words) . lines $ string

makeState :: ProgId -> [Instruction] -> State
makeState i is = State {progId = i, pointer = Just 0, instructions = is, processor = Map.singleton "p" i, sent = [], received = []}

runSolo :: State -> State
runSolo state@(State {pointer = Nothing}) = state
runSolo state@(State {pointer = (Just p), instructions = is, received = rcv})
  | rcv /= [] = state
  | otherwise = runSolo $ (is !! p) state

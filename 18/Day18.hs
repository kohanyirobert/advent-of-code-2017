module Day18 where

import qualified Data.Map.Strict as Map

import Data.Char (isAlpha)

type Register = String
type Operator = String
type Soundcard = [Int]
type Backup = [Int]
type Processor = Map.Map String Int
type Pointer = Int
type Offset = Int
type Instruction = State -> State
type Predicate = State -> Bool

data State = State { pointer :: Maybe Pointer
                   , instructions :: [Instruction]
                   , soundcard :: Soundcard
                   , processor :: Processor
                   , backup :: Backup
                   }

instance Show State where
  show (State {pointer = p, soundcard = sc, processor = proc, backup = b}) =
    "State {pointer = " ++ show p ++
    ", soundcard = " ++ show sc ++
    ", processor = " ++ show proc ++ 
    ", backup = " ++ show b ++ 
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
sndRegister r state@(State {soundcard = sc, processor = proc}) =
  let sc' = (Map.findWithDefault 0 r proc) : sc
  in state {soundcard = sc'}

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
rcvRegister r state@(State {soundcard = sc, processor = proc, backup = b}) =
  if coerceToValue r proc == 0
  then state
  else state {backup = head sc : b}

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

makeState :: [Instruction] -> State
makeState instructions = State (Just 0) instructions [] Map.empty []

runInstructions :: Predicate -> State -> State
runInstructions _ state@(State {pointer = Nothing}) = state
runInstructions predicate state@(State {pointer = (Just p), instructions = is})
  | predicate state = state
  | otherwise = runInstructions predicate $ (is !! p) state

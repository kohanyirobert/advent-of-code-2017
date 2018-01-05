module Day18 where

import qualified Data.Map.Strict as Map

import Data.Char (isAlpha)
import Debug.Trace (traceShow)

type Program = Int
type Register = String
type Operand = String
type Operator = String
type Queue = [Int]
type Processor = Map.Map String Int
type Pointer = Int
type Offset = Int
type Predicate = State -> Bool
type Transform = ([State] -> [State])

data Instruction
  = UnaryInstruction Operator Operand Transform
  | BinaryInstruction Operator Operand Operand Transform

instance Show Instruction where
  show (UnaryInstruction o a _) = "UnaryInstruction " ++ show o ++ " " ++ show a
  show (BinaryInstruction o a b _) = "BinaryInstruction " ++ show o ++ " " ++ show a ++ " " ++ show b

data State = State
  { program :: Program
  , pointer :: Maybe Pointer
  , instructions :: [Instruction]
  , processor :: Processor
  , sent :: Queue
  , received :: Queue
  } deriving (Show)

isRegister :: Operand -> Bool
isRegister = all isAlpha

coerceToValue :: String -> Processor -> Int
coerceToValue s p
  = if isRegister s
    then Map.findWithDefault 0 s p
    else read s

movePointer :: Offset -> [State] -> [State]
movePointer offset (s : ss)
  = let (Just p) = pointer s
        is = instructions s
        p' = p + offset
        size = length is
    in if 0 <= p' && p' <= size - 1
       then s {pointer = Just p'} : ss
       else s {pointer = Nothing} : ss

jgzPointer :: Operand -> Operand -> [State] -> [State]
jgzPointer a b state@(s : ss)
  = let proc = processor s
        offset = if coerceToValue a proc == 0
                 then 1
                 else coerceToValue b proc
    in movePointer offset state

sndRegister :: Operand -> [State] -> [State]
sndRegister a (s : [])
  = let snd = sent s
        x = coerceToValue a (processor s)
    in [s {sent = x : snd}]
sndRegister a (s0 : s1 : [])
  = let snd0 = sent s0
        rcv1 = received s1
        x0 = coerceToValue a (processor s0)
    in [s0 {sent = x0 : snd0}, s1 {received = x0 : rcv1}]

rcvRegister :: Operand -> [State] -> [State]
rcvRegister a (s : [])
  = let x = coerceToValue a (processor s)
        snd = sent s
        rcv = received s
    in if x == 0
       then [s]
       else [s {sent = tail snd, received = head snd : rcv}]
rcvRegister r (s0 : s1 : [])
  = let rcv0 = received s0
        x = head rcv0
        (s0' : []) = setRegister r (show x) [s0]
    in [s0', s1]

setRegister :: Register -> Operand -> [State] -> [State]
setRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.insert r (coerceToValue b proc) proc
    in s {processor = proc'} : ss

addRegister :: Register -> Operand -> [State] -> [State]
addRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a + coerceToValue b proc) r proc
    in s {processor = proc'} : ss

mulRegister :: Register -> Operand -> [State] -> [State]
mulRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a * coerceToValue b proc) r proc
    in s {processor = proc'} : ss

modRegister :: Register -> Operand -> [State] -> [State]
modRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a `mod` coerceToValue b proc) r proc
    in s {processor = proc'} : ss

stringToInstruction :: [String] -> Instruction
stringToInstruction (o : a : []) =
  UnaryInstruction o a $ case o of
    "snd" -> movePointer 1 . sndRegister a
    "rcv" -> movePointer 1 . rcvRegister a
stringToInstruction (o : a : b : []) =
  BinaryInstruction o a b $ case o of
    "set" -> movePointer 1 . setRegister a b
    "add" -> movePointer 1 . addRegister a b
    "mul" -> movePointer 1 . mulRegister a b
    "mod" -> movePointer 1 . modRegister a b
    "jgz" -> jgzPointer a b

getInstructions :: String -> [Instruction]
getInstructions string = map (stringToInstruction . words) . lines $ string

makeState :: Program -> [Instruction] -> State
makeState i is
  = State { program = i
          , pointer = Just 0
          , instructions = is
          , processor = Map.singleton "p" i
          , sent = []
          , received = []
          }

runInstructions :: [State] -> [State]
runInstructions (s : [])
  | rcv /= [] = [s]
  | otherwise = let is = instructions s
                    (Just p) = pointer s
                    i = is !! p
                    f' = case i of (UnaryInstruction _ _ f) -> f
                                   (BinaryInstruction _ _ _ f) -> f
                    (s' : []) = f' [s]
                in runInstructions [s']
  where rcv = received s
runInstructions (s0 : s1 : [])
  = let is0 = instructions s0
        (Just p0) = pointer s0
        i = is0 !! p0
        f' = case i of (UnaryInstruction _ _ f) -> f
                       (BinaryInstruction _ _ _ f) -> f
    in f' [s0, s1]

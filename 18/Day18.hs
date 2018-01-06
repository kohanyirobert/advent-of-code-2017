module Day18 where

import qualified Data.Map.Strict as Map

import Data.Char (isAlpha)
import Data.Maybe (fromJust)

type Program = Int
type Register = String
type Operand = String
type Operator = String
type Queue = [Int]
type Processor = Map.Map String Int
type Pointer = Int
type Offset = Int
type Transform = ([State] -> [State])

data Status = Running | Waiting
  deriving (Eq, Show)

data Instruction
  = UnaryInstruction { operator :: Operator
                     , operand :: Operand
                     , transform :: Transform
                     }
  | BinaryInstruction { operator :: Operator
                      , operandA :: Operand
                      , operandB :: Operand
                      , transform :: Transform
                      }

instance Show Instruction where
  show (UnaryInstruction {operator = o, operand = a})
    = "UnaryInstruction {operator = " ++ show o ++ " operand = " ++ show a ++ "}"
  show (BinaryInstruction {operator = o, operandA = a, operandB = b})
    = "BinaryInstruction {operator = " ++ show o ++ " operandA = " ++ show a ++ " operandB = " ++ show b ++ "}"

data State = State
  { program :: Program
  , status :: Status
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
        offset = if coerceToValue a proc > 0
                 then coerceToValue b proc
                 else 1
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
    in [s0 {sent = snd0 ++ [x0]}, s1 {received = rcv1 ++ [x0]}]

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
    in if null rcv0
       then [s0 {status = Waiting}, s1]
       else let x = head rcv0
                (s0' : []) = setRegister r (show x) [s0]
                in movePointer 1 [s0' {status = Running, received = tail rcv0}, s1]

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
    "rcv" -> rcvRegister a
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
          , status = Running
          , pointer = Just 0
          , instructions = is
          , processor = Map.singleton "p" i
          , sent = []
          , received = []
          }

findState :: Program -> [State] -> Maybe State
findState _ [] = Nothing
findState i (s : ss)
  | i == program s = Just s
  | otherwise = findState i ss

isWaiting :: [State] -> Bool
isWaiting states = all (\s -> status s == Waiting) states

runInstructions :: [State] -> [State]
runInstructions states@(s : [])
  | p == Nothing || rcv /= [] = states
  | otherwise = runInstructions [s']
  where rcv = received s
        is = instructions s
        p = pointer s
        i = is !! (fromJust p)
        f' = transform i
        (s' : []) = f' [s]
runInstructions states@(s : ss)
  | isWaiting states = states
  | otherwise = runInstructions . reverse . f $ states
  where (Just p) = pointer s
        is = instructions s
        i = is !! p
        f = transform i

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
type Statistics = Map.Map Operand Int
type Predicate = [State] -> Bool

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
  , statistics :: Statistics
  } deriving (Show)

isRegister :: Operand -> Bool
isRegister = all isAlpha

coerceToValue :: String -> Processor -> Int
coerceToValue s p
  = if isRegister s
    then Map.findWithDefault 0 s p
    else read s

movePointer :: Offset -> State -> State
movePointer offset s
  = let (Just p) = pointer s
        is = instructions s
        p' = p + offset
        size = length is
    in if 0 <= p' && p' <= size - 1
       then s {pointer = Just p'}
       else s {pointer = Nothing}

jgzPointer :: Operand -> Operand -> [State] -> [State]
jgzPointer a b state@(s : ss)
  = let proc = processor s
        offset = if coerceToValue a proc > 0
                 then coerceToValue b proc
                 else 1
    in movePointer offset s : ss

jnzPointer :: Operand -> Operand -> [State] -> [State]
jnzPointer a b state@(s : ss)
  = let proc = processor s
        offset = if coerceToValue a proc /= 0
                 then coerceToValue b proc
                 else 1
    in movePointer offset s : ss

sndRegister :: Operand -> [State] -> [State]
sndRegister a (s : [])
  = let snd = sent s
        x = coerceToValue a (processor s)
    in [movePointer 1 s {sent = x : snd}]
sndRegister a (s0 : s1 : [])
  = let snd0 = sent s0
        rcv1 = received s1
        x0 = coerceToValue a (processor s0)
        s0' = movePointer 1 s0 {sent = snd0 ++ [x0]}
        s1' = s1 {received = rcv1 ++ [x0]}
    in [s0', s1']

rcvRegister :: Operand -> [State] -> [State]
rcvRegister a (s : [])
  = let x = coerceToValue a (processor s)
        snd = sent s
        rcv = received s
    in [movePointer 1 $ if x == 0
                        then s
                        else s {sent = tail snd, received = head snd : rcv}]
rcvRegister r (s : ss)
  = let rcv = received s
    in if null rcv
       then s {status = Waiting} : ss
       else let proc = processor s
                proc' = Map.insert r (head rcv) proc
                s' = movePointer 1 s {status = Running, processor = proc', received = tail rcv}
                in s' : ss

setRegister :: Register -> Operand -> [State] -> [State]
setRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.insert r (coerceToValue b proc) proc
        s' = movePointer 1 s {processor = proc'}
    in s' : ss

addRegister :: Register -> Operand -> [State] -> [State]
addRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a + coerceToValue b proc) r proc
        s' = movePointer 1 s {processor = proc'}
    in s' : ss

subRegister :: Register -> Operand -> [State] -> [State]
subRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a - coerceToValue b proc) r proc
        s' = movePointer 1 s {processor = proc'}
    in s' : ss

mulRegister :: Register -> Operand -> [State] -> [State]
mulRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a * coerceToValue b proc) r proc
        s' = movePointer 1 s {processor = proc'}
    in s' : ss

modRegister :: Register -> Operand -> [State] -> [State]
modRegister r b (s : ss)
  = let proc = processor s
        proc' = Map.adjust (\a -> a `mod` coerceToValue b proc) r proc
        s' = movePointer 1 s {processor = proc'}
    in s' : ss

updateStatistics :: Operator -> [State] -> [State]
updateStatistics operator (s : ss)
  = let stats = statistics s
        stats' = if Map.member operator stats
                 then Map.adjust (+1) operator stats
                 else Map.insert operator 1 stats
        s' = s {statistics = stats'}
    in s' : ss

stringToInstruction :: [String] -> Instruction
stringToInstruction (o : a : []) =
  UnaryInstruction o a $ case o of
    "snd" -> updateStatistics o . sndRegister a
    "rcv" -> updateStatistics o . rcvRegister a
stringToInstruction (o : a : b : []) =
  BinaryInstruction o a b $ case o of
    "set" -> updateStatistics o . setRegister a b
    "add" -> updateStatistics o . addRegister a b
    "sub" -> updateStatistics o . subRegister a b
    "mul" -> updateStatistics o . mulRegister a b
    "mod" -> updateStatistics o . modRegister a b
    "jgz" -> updateStatistics o . jgzPointer a b
    "jnz" -> updateStatistics o . jnzPointer a b

getInstructions :: String -> [Instruction]
getInstructions string = map (stringToInstruction . words) . lines $ string

makeState :: Bool -> Program -> [Instruction] -> State
makeState debug i is
  = State { program = i
          , status = Running
          , pointer = Just 0
          , instructions = is
          , processor = if debug
                        then Map.fromList [("a", 1), ("p", i)]
                        else Map.singleton "p" i
          , sent = []
          , received = []
          , statistics = Map.empty
          }

findState :: Program -> [State] -> Maybe State
findState _ [] = Nothing
findState i (s : ss)
  | i == program s = Just s
  | otherwise = findState i ss

runInstructions :: Predicate -> [State] -> [State]
runInstructions isDone states@(s : [])
  | isDone states = states
  | otherwise = runInstructions isDone . f $ states
  where is = instructions s
        (Just p) = pointer s
        i = is !! p
        f = transform i
runInstructions isDone states@(s : ss)
  | isDone states = states
  | otherwise = runInstructions isDone . reverse . f $ states
  where (Just p) = pointer s
        is = instructions s
        i = is !! p
        f = transform i

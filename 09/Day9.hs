module Day9 where

type Count = Int
type Depth = Int

data State = Group Count Depth | Garbage Count Depth
  deriving Show

getChars :: String -> String
getChars string = head $ lines string

countGroups' :: State -> String -> State
countGroups' state [] = state
countGroups' state ('!' : char : chars) = countGroups' state chars
countGroups' state@(Garbage count depth) (char : chars)
  | char == '>' = countGroups' (Group count depth) chars
  | otherwise = countGroups' state chars
countGroups' state@(Group count depth) (char : chars)
  | char == '{' = countGroups' (Group count (depth + 1)) chars
  | char == '}' = countGroups' (Group (count + depth) (depth - 1)) chars
  | char == ',' = countGroups' state chars
  | char == '<' = countGroups' (Garbage count depth) chars
  | otherwise = state

countGroups :: String -> Int
countGroups string =
  let (Group count _) = countGroups' (Group 0 0) string
  in count

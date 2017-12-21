module Day12 where

import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust)

type Id = Int
type Count = Int

data Program = Program Id [Id]
  deriving (Eq, Show)

data Node = Node Id [Node]
  deriving (Eq, Show)

getPrograms :: String -> [Program]
getPrograms string = map toProgram dataRecords
  where toDataLine = \line -> filter (\char -> isDigit char || isSpace char) line
        dataLines = map toDataLine $ lines string
        dataRecords = map words dataLines
        toProgram = \(programId : connectedIds) -> Program (read programId) (map read connectedIds)

findProgram :: Id -> [Program] -> Maybe Program
findProgram _ [] = Nothing
findProgram targetId (program@(Program programId _) : programs)
  | targetId == programId = Just program
  | otherwise = findProgram targetId programs

buildGraph' :: [Program] -> Program -> [Program] -> Node
buildGraph' seenPrograms program@(Program programId connectedIds) allPrograms = Node programId connectedNodes
  where newSeenPrograms = program : seenPrograms
        toProgram = \neighborId -> fromJust $ findProgram neighborId allPrograms
        connectedPrograms = map toProgram connectedIds
        unseenPrograms = filter (`notElem` newSeenPrograms) connectedPrograms
        toNode = \connectedProgram -> buildGraph' newSeenPrograms connectedProgram allPrograms
        connectedNodes = map toNode unseenPrograms

buildGraph :: Program -> [Program] -> Node
buildGraph program allPrograms = buildGraph' [] program allPrograms

collectIds' :: Node -> [Id] -> [Id]
collectIds' node@(Node id nodes) ids = uniqueIds
  where allIds = id : foldl (\b a -> b ++ (collectIds' a ids)) ids nodes
        uniqueIds = foldl (\b a -> if a `elem` b then b else a : b) [] allIds

collectIds :: Node -> [Id]
collectIds node = collectIds' node []

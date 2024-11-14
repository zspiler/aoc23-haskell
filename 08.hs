import System.Environment (getArgs)
import Utils (slice)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Debug.Trace (trace)

start = "AAA"
goal = "ZZZ"

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let moves = cycle (head $ lines contents)
    let nodeMap = parseNodes $ lines contents

    print $ stepsToEnd nodeMap "AAA" moves 0

parseNodes :: [String] -> Map.Map String (String, String)
parseNodes lns = Map.fromList [(slice 0 2 line, (slice 7 9 line, slice 12 14 line)) | line <- tail $ tail lns]        

stepsToEnd :: Map.Map String (String, String) -> String -> String -> Int -> Int
stepsToEnd nodeMap currentNode moves count
    | nextNode == goal = count + 1
    | otherwise = stepsToEnd nodeMap nextNode moves (count + 1)
    where nextNode = getNextNode currentNode (moves !! count) nodeMap

getNextNode :: String -> Char -> Map.Map String (String, String) -> String
getNextNode node move nodeMap = case Map.lookup node nodeMap of
    Just neighbors -> if move == 'L' then fst neighbors else snd neighbors
    Nothing -> error "🙀"

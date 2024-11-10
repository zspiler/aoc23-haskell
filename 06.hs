import System.Environment (getArgs)
import Data.List (tail, find)
import Utils (readInt, tuplify3, mapTuple)
import Data.Maybe (fromJust, isJust)

type Race = (Int, Int) -- Time, Distance

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    
    let races = parseRaces $ lines contents
    print $ part1 races
    print $ part2 races

part1 :: [Race] -> Int
part1 races = product $ map pressesToBeatRecord races

part2 :: [Race] -> Int
part2 races = product $ map pressesToBeatRecord [combineRaces races]

pressesToBeatRecord :: Race -> Int
pressesToBeatRecord (recordTime, distance) = length $ filter (>distance) $ map (`result` recordTime) [1..recordTime]
    where result pressTime recordTime = (recordTime - pressTime) * pressTime

parseRaces :: [String] -> [Race]
parseRaces lns = zip (parseNums times) (parseNums distances)
    where
        parseNums = map readInt . tail . words
        times = head lns
        distances = lns !! 1
        
combineRaces :: [Race] -> Race
combineRaces races = (combine $ map fst races, combine $ map snd races)
    where
        combine nums = read (concatMap show nums)
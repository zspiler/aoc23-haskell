import System.Environment (getArgs)
import Data.List (tail, find)
import Utils (readInt, tuplify3)
import Data.Maybe (fromJust, isJust)

type Mapping = (Int, Int, Int)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let seeds = parseSeeds . head $ lines contents
    let groups = parseGroups . tail $ lines contents

    print $ part1 seeds groups

part1 :: [Int] -> [[Mapping]] -> Int
part1 seeds categories = minimum (map (\seed -> foldl applyFirstValidMapping seed categories) seeds)

applyFirstValidMapping :: Int -> [Mapping] -> Int
applyFirstValidMapping seed mappings =  
    case validMapping of 
        Just mapping -> fromJust (mapSeed seed mapping)
        Nothing -> seed
    where validMapping = find (isJust . mapSeed seed) mappings

mapSeed ::  Int -> Mapping -> Maybe Int
mapSeed seed (destStart, srcStart, len)
    | seed < srcStart || seed > srcStart + len = Nothing
    | mapped < destStart || mapped > destStart + len = Nothing
    | otherwise = Just mapped
    where mapped = destStart + seed - srcStart

parseSeeds :: String -> [Int]
parseSeeds = map readInt . drop 1 . words

parseGroups :: [String] -> [[Mapping]]
parseGroups lines = map tail (tuplify group)
    where   tuplify = map (map tuplify3)
            group = foldl (\acc l ->  if null l then acc++[[]] else init acc ++ [last acc ++ [map readInt $ words l]]) [] lines

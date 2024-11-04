import System.Environment (getArgs)
import Data.Char (isDigit, intToDigit)
import Data.List (findIndices)
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe)
import Utils (firstAndLast, readInt, removeSpaces, splitBy, reverseTuple, indices, slice, enumerated, areAdjacent, adjacentCoordinates, getAtCoordinate)
import qualified Data.Map as Map

type Schematic = [String]

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let schematic = lines contents
    print (solve1 schematic)
    print (solve2 schematic)

solve1 :: Schematic -> Int
solve1 schematic = sum (concat (Map.elems (numbersPerSymbol schematic)))

solve2 :: Schematic -> Int
solve2 schematic = sum (Map.mapWithKey (\_ value -> product value) filterGears)
    where
        filterGears = Map.filter (\v -> length v == 2) (numbersPerSymbol schematic)

numbersPerSymbol :: Schematic -> Map.Map (Int, Int) [Int]
numbersPerSymbol schematic = Map.fromList symbolNumbersList
    where
            symbolNumbersList = map (\(symbol, nums) -> (symbol, map (readNumber schematic) nums)) symbolsList
            symbolsList = map (\symbol -> (symbol, filter (isNextToSymbol symbol) numbers)) symbols
            symbols = findSymbols schematic
            numbers = findNumbers schematic
            isNextToSymbol = any . areAdjacent
            readNumber schematic coords = readInt(map (getAtCoordinate schematic) coords)

findSymbols :: Schematic -> [(Int, Int)]
findSymbols schematic = concatMap findSymbolIndices (enumerated schematic)
    where 
        findSymbolIndices (y, line) = map (, y) (findIndices isSymbol line)
        isSymbol c = not (isDigit c) && c /= '.'

findNumbers :: Schematic -> [[(Int, Int)]]
findNumbers schematic = concatMap (\(y, line) -> findNumbersInLine y line) (enumerated schematic)

findNumbersInLine y str = filter (not . null) (map extractNumber (enumerated str))
    where extractNumber (i, c)
            | isDigit c && (i == 0 || not (isDigit (str !! (i-1)))) = toCoordinates (i, length (takeWhile isDigit (drop i str)))
            | otherwise = []
                where toCoordinates (start, len) = map (, y) [start..(start + len - 1)]
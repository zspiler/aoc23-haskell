import System.Environment (getArgs)
import Utils (gridCoordinates, getAtCoordinate, untilStable)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = lines contents
    print $ totalLoad $ untilStable tilt grid


totalLoad :: [String] -> Int
totalLoad grid = sum $ map (\(_,y) -> length grid - y) (findRoundRocks grid)

tilt :: [String] -> [String]
tilt grid = [[getAt (x,y) | x <- [0..width-1]] | y <- [0..height-1]]
    where
        getAt (x, y)
            | (x, y) `elem` rolledRoundRocks = 'O'
            | (x, y) `elem` emptySpaces = '.'
            | (x, y) `elem` cubeRocks = '#'
            | otherwise = error "?🐭?"
        rolledRoundRocks = map (\(x, y) -> if y > 0 && getAtCoordinate grid (x, y-1) == '.' then (x, y-1) else (x,y)) $ findRoundRocks grid
        emptySpaces = findEmptySpaces grid
        cubeRocks = findCubeRocks grid
        height = length grid
        width = length $ head grid

findRoundRocks :: [String] -> [(Int, Int)]
findRoundRocks grid = filterCoordinates grid ['O']

findCubeRocks :: [String] -> [(Int, Int)]
findCubeRocks grid = filterCoordinates grid ['#']

findEmptySpaces :: [String] -> [(Int, Int)]
findEmptySpaces grid = filterCoordinates grid ['O', '.']

filterCoordinates :: [String] -> [Char] -> [(Int, Int)]
filterCoordinates grid elems = filter (\(x, y) -> getAtCoordinate grid (x, y) `elem` elems) $ gridCoordinates grid
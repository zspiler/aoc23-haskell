import System.Environment (getArgs)
import Utils (indices, gridCoordinates, getAtCoordinate)
import Data.List (all)

expansionFactor = 10^6

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = lines contents
    let galaxies = findGalaxies grid
    let galaxyCombos = [(galaxy1, galaxy2) | galaxy1 <- galaxies, galaxy2 <- galaxies, galaxy1 /= galaxy2, galaxy1 < galaxy2]
    let emptyRows = findEmptyRows grid
    let emptyCols = findEmptyCols grid

    print $ sum $ map (\(galaxy1, galaxy2) -> lengthOfShortestPath galaxy1 galaxy2 grid emptyRows emptyCols) galaxyCombos

lengthOfShortestPath :: (Int, Int) -> (Int, Int) -> [String] -> [Int] -> [Int] -> Int
lengthOfShortestPath (x1, y1) (x2, y2) grid emptyRows emptyCols = adjustedDiff x1 x2 emptyCols + adjustedDiff y1 y2 emptyRows

adjustedDiff :: Int -> Int -> [Int] -> Int
adjustedDiff x1 x2 empty = sum weightedSteps
    where weightedSteps = map (\x -> if x `elem` empty then expansionFactor else 1) [min x1 x2 .. max x1 x2 - 1]

findEmptyRows :: [String] -> [Int]
findEmptyRows grid = filter (\y -> all (=='.') (grid !! y)) (indices grid)

findEmptyCols :: [String] -> [Int]
findEmptyCols grid = filter (all (=='.') . column) (indices $ head grid)
    where column x = map (\y -> getAtCoordinate grid (x,y)) (indices grid)

findGalaxies :: [String] -> [(Int, Int)]
findGalaxies grid = filter ((== '#') . getAtCoordinate grid) (gridCoordinates grid)
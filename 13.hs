import System.Environment (getArgs)
import Data.List(findIndex, all, find)
import Utils (indices, slice, splitByEmptyLines)
import Data.Maybe (fromJust, isJust)

type VerticalAndHorizontalPoints = (Maybe Int, Maybe Int)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let grids = splitByEmptyLines $ lines contents
    
    print $ sum $ map (toSummary . findHorizontalAndVerticalPoints) grids

toSummary :: VerticalAndHorizontalPoints -> Int
toSummary (horizontalPoint, verticalPoint)
    | isJust horizontalPoint = 100 * fromJust horizontalPoint
    | isJust verticalPoint = fromJust verticalPoint
    | otherwise = error "😔"

findHorizontalAndVerticalPoints :: [String] -> VerticalAndHorizontalPoints
findHorizontalAndVerticalPoints grid = (fmap (+1) horizontal, fmap (+1) vertical)
    where horizontal = find (findHorizontalPoint grid) [0..height-2]
          vertical = find (findVerticalPoint grid) [0..width-2]
          height = length grid
          width = length $ head grid

findHorizontalPoint :: [String] -> Int -> Bool
findHorizontalPoint grid y = firstNormalized == reverse secondNormalized
    where
        firstNormalized = if length first < length second then first else drop (length first - length second) first
        secondNormalized = if length first < length second then take (length first) second  else second
        first = slice 0 y grid
        second = slice (y+1) (length grid) grid

findVerticalPoint :: [String] -> Int -> Bool
findVerticalPoint grid = findHorizontalPoint $ transpose grid

transpose :: [String] -> [String]
transpose grid = map (\x -> map (\y -> grid !! y !! x) (indices grid)) (indices $ head grid)
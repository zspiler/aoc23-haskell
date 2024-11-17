import System.Environment (getArgs)
import Utils (slice, getAtCoordinate)
import Data.Maybe (fromJust)
import Data.List (all, find)
import qualified Data.Map as Map

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = lines contents
    let firstMove = (62, 64)
    let start = findStart grid

    let path = walk grid firstMove [findStart grid]
    let stepsToFarthestPoint = fromIntegral (length path + 1) / 2
    print stepsToFarthestPoint

findStart :: [String] -> (Int, Int)
findStart grid = fromJust $ find (\pos -> getAtCoordinate grid pos  == 'S') coordinates
    where coordinates = [(x,y) | y <- [0..length grid - 1], x <- [0..length (head grid)-1]]

walk :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
walk grid pos path
    | null nextPipes = path
    | otherwise = walk grid (head nextPipes) (path ++ [pos])
    where
        nextPipes = filter (`notElem` path) (neighbourPipes grid pos)
        element = getAtCoordinate grid pos

neighbourPipes :: [String] -> (Int, Int) -> [(Int, Int)]
neighbourPipes grid pos 
    | element `elem` ['.', 'S'] = []
    | otherwise = map (applyDirection pos) (fromJust $ Map.lookup element pipeData) 
    where
        element = getAtCoordinate grid pos
        pipeData = Map.fromList [('|', "NS"),('L', "NE"),('J', "NW"),('-', "EW"),('7', "SW"),('F', "SE")]                 

applyDirection :: (Int, Int) -> Char -> (Int, Int)
applyDirection (x, y) direction
    | direction == 'N' = (x, y-1)
    | direction == 'S' = (x, y+1)
    | direction == 'E' = (x+1, y)
    | direction == 'W' = (x-1, y)
    | otherwise = error "🥺"
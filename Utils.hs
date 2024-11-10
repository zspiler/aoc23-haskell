module Utils (readInt, indices, firstAndLast, removeSpaces, splitBy, reverseTuple, slice, enumerated, areAdjacent, adjacentCoordinates, getAtCoordinate, tuplify2, tuplify3) where

readInt s = read s :: Int

indices s = take (length s) [0..]

firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast [x] = [x, x]
firstAndLast x = [head x, last x]

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

splitBy :: Char -> String -> [String]
splitBy delim = filter (not . null) . foldl (cb delim) []
    where cb delim acc c
            | null acc = [[c]]
            | c == delim = acc++[""]
            | otherwise = init acc ++ [last acc ++ [c]]
            

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

slice :: Int -> Int -> [a] -> [a]
slice start end s = take (end - start + 1) (drop start s)

enumerated :: [a] -> [(Int, a)]
enumerated = zip [0..]


-- coordinates

areAdjacent :: (Int, Int) -> (Int, Int) -> Bool
areAdjacent (x0, y0) (x1, y1) = abs (x0 - x1) <= 1 && abs (y0 - y1) <= 1

adjacentCoordinates :: (Int, Int) -> [(Int, Int)]
adjacentCoordinates (x, y) = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1]]

getAtCoordinate :: [[a]] -> (Int, Int) -> a
getAtCoordinate grid (x, y) = grid !! y !! x
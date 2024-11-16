import System.Environment (getArgs)
import Utils (slice, readInt, indices)
import Data.Maybe (fromJust)
import Data.List (all)
import qualified Data.Map as Map

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let lns = lines contents

    print $ sum $ map ((\history -> nextValue 0 (sequences [history])) . parseHistory) lns
    print $ sum $ map ((\history -> prevValue 0 (sequences [history])) . parseHistory) lns

nextValue :: Num a => Int -> [[a]] -> a
nextValue rowIndex sequences
    | rowIndex < length sequences - 1 = (row !! (length row-1)) + nextValue (rowIndex+1) sequences
    | otherwise = 0
    where row = sequences !! rowIndex

prevValue :: Num a => Int -> [[a]] -> a
prevValue rowIndex sequences
    | rowIndex < length sequences - 1 = head row - prevValue (rowIndex+1) sequences
    | otherwise = 0
    where row = sequences !! rowIndex

sequences :: (Eq a, Num a) => [[a]] -> [[a]]
sequences nums
    | all (== 0) lastDiffs = nums ++ [lastDiffs]
    | otherwise = sequences $ nums ++ [lastDiffs]
    where lastDiffs = diffs $ last nums

diffs :: (Num a) => [a] -> [a]
diffs nums = map (\i -> (nums !! (i+1)) - (nums !! i)) (init $ indices nums)

parseHistory :: String -> [Int]
parseHistory line = map readInt (words line)
import System.Environment (getArgs)
import Utils (splitBy, readInt, indices, isBitSet)
import Data.List (elemIndices, elemIndex)

type SpringsWithGroups = (String, [Int])

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let parsedRows = map parseRow (lines contents)
    print $ sum $ map (\(springs, groups) -> length $ filter (matchesGroups groups) (combos springs)) parsedRows

combos :: String -> [String]
combos springs = map (createCombo springs) ([0..((2^undeterminedCount)-1)] :: [Int])
    where
        createCombo springs comboNum = map (replaceChar comboNum) (indices springs)
        replaceChar comboNum index = if (springs !! index) == '?' then generateChar (findUndeterminedIndex springs index) comboNum else springs !! index
        generateChar undeterminedIndex comboNum = if isBitSet comboNum undeterminedIndex then '#' else '.'
        undeterminedCount = length $ filter (=='?') springs
        findUndeterminedIndex spring i = case elemIndex i (elemIndices '?' spring) of
            Just idx -> idx
            Nothing -> error "Index not found"

matchesGroups :: [Int] -> String -> Bool
matchesGroups groups springs = groups == map length (splitBy '.' springs)

parseRow :: String -> SpringsWithGroups
parseRow row = (head split, map readInt (splitBy ',' (split !! 1)))
    where split = words row
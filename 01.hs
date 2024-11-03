import System.Environment (getArgs)
import Data.Char (isDigit, intToDigit)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Utils (firstAndLast, readInt)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    print . solvePart1 . lines $ contents
    print . solvePart2 . lines $ contents

isSubstringAt :: String -> String -> Int -> Bool
isSubstringAt str substr i
    | null substr = True
    | i >= length str = False
    | take (length substr) (drop i str) == substr = True
    | otherwise = False

wordSubstring str i = find (\(word, digit) -> isSubstringAt str word i) words
    where words = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]

extractDigits :: String -> String
extractDigits line = filter (/= ' ') $ map toDigit zipped
    where
        toDigit (i, x)
            | isDigit x = x
            | isJust digitWord = intToDigit . snd . fromJust $ digitWord
            | otherwise = ' '
            where
                digitWord = wordSubstring line i
        zipped = zip [0..] line

calibrationValue line = readInt (firstAndLast . filter isDigit $ line)

solvePart1 lines = sum $ map calibrationValue lines
solvePart2 lines = sum $ map (calibrationValue . extractDigits) lines

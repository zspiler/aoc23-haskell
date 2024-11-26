import System.Environment (getArgs)
import Data.Char (ord)
import Utils (splitBy)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let sequence = splitBy ',' contents

    print $ sum $ map hash sequence

hash :: String -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0
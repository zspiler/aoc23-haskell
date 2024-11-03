module Utils (readInt, indices, firstAndLast, removeSpaces, splitBy, reverseTuple) where

readInt s = read s :: Int

indices s = take (length s) [0..]

firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast [x] = [x, x]
firstAndLast x = [head x, last x]

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

splitBy :: Char -> String -> [String]
splitBy delim = foldl (cb delim) []
    where cb delim acc c
            | null acc = [[c]]
            | c == delim = acc++[""]
            | otherwise = init acc ++ [last acc ++ [c]]


reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)
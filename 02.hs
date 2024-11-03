import System.Environment (getArgs)
import Data.Char (isDigit, intToDigit)
import Data.Maybe (fromJust, isJust)
import Utils (firstAndLast, readInt, removeSpaces, splitBy, reverseTuple)
import qualified Data.Map as Map

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    print . solve1 . lines $ contents
    print . solve2 . lines $ contents

availableCubes :: Map.Map String Int
availableCubes = Map.fromList [("red", 12),("green", 13), ("blue", 14)]

solve1 :: [String] -> Int
solve1 lines = sum (map fst (filter (\(i, line) -> isGamePossible (parseGame line)) zipped))
    where zipped = zip [1..] lines

solve2 :: [String] -> Int
solve2 lines = sum (map (gamePower . parseGame) lines)

isGamePossible :: [[(Int, String)]] -> Bool
isGamePossible = all isRoundValid

isRoundValid :: [(Int, String)] -> Bool
isRoundValid = all isMoveValid

isMoveValid :: (Int, String) -> Bool
isMoveValid (amount, color) =  case val of
    Just x -> x >= amount
    Nothing -> False
    where val = Map.lookup color availableCubes

parseGame :: String -> [[(Int, String)]]
parseGame game = map parseRound round
    where round = splitBy ';' (dropTitle game)
          dropTitle s = filter (/= ':') (dropWhile (/= ':') s)

parseRound :: String -> [(Int, String)]
parseRound round = map parsePair splitStr
    where
        parsePair [val, color] = (readInt (removeSpaces val), removeSpaces color)
        splitStr = map (splitBy ' ') (splitBy ',' round)

gamePower :: [[(Int, String)]] -> Int
gamePower game = product (Map.elems dict)
    where dict = Map.fromListWith max (map reverseTuple (concat game))
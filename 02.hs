import System.Environment (getArgs)
import Data.Char (isDigit, intToDigit)
import Data.Maybe (fromJust, isJust)
import Utils (firstAndLast, readInt, removeSpaces, splitBy, reverseTuple)
import qualified Data.Map as Map

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

type Game = [Round]
type Round = [Move]
type Move = (Int, Color)

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    print . solve1 . lines $ contents
    print . solve2 . lines $ contents

solve1 :: [String] -> Int
solve1 lines = sum (map fst (filter (\(i, line) -> isGamePossible (parseGame line)) zipped))
    where zipped = zip [1..] lines

solve2 :: [String] -> Int
solve2 lines = sum (map (gamePower . parseGame) lines)

availableCubes :: Map.Map Color Int
availableCubes = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

isGamePossible :: Game -> Bool
isGamePossible = all isRoundValid

isRoundValid :: Round -> Bool
isRoundValid = all isMoveValid

isMoveValid :: Move -> Bool
isMoveValid (amount, color) =  case val of
    Just x -> x >= amount
    Nothing -> False
    where val = Map.lookup color availableCubes

gamePower :: Game -> Int
gamePower game = product (Map.elems dict)
    where dict = Map.fromListWith max (map reverseTuple (concat game))

parseGame :: String -> [[Move]]
parseGame game = map parseRound round
    where round = splitBy ';' (dropTitle game)
          dropTitle s = filter (/= ':') (dropWhile (/= ':') s)

parseRound :: String -> [Move]
parseRound round = map parsePair splitStr
    where
        parsePair [v, c] = case parseColor(removeSpaces c) of
            Just color -> (readInt (removeSpaces v), color)
            Nothing -> error ("Invalid color: " ++ c)
        splitStr = map (splitBy ' ') (splitBy ',' round)

parseColor :: String -> Maybe Color
parseColor s = case s of 
    "red" -> Just Red
    "green" -> Just Green
    "blue" -> Just Blue
    _ -> Nothing
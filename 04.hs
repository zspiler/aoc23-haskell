import System.Environment (getArgs)
import Data.List (intersect, elemIndex, tail)
import Utils (readInt, tuplify2, splitBy, removeSpaces, indices)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Debug.Trace (trace)

type Card = ([Int], [Int])

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let cards = map parseCard $ lines contents

    print $ part1 cards
    print $ part2 cards

part1 :: [Card] -> Int
part1 cards = sum (map calculateCardPoints cards)

part2 :: [Card] -> Int
part2 cards = sum $ map (calculateCardCopies (map calculateCardMatches cards)) (indices cards)

calculateCardCopies :: [Int] -> Int -> Int
calculateCardCopies matches i = result
    where
        result = 1 + sum (map (calculateCardCopies matches) copies)
        copies = take (matches !! i) [(i+1)..]

calculateCardPoints :: Card -> Int
calculateCardPoints cards@(winningCards, myCards) = points matchingCards
    where   matchingCards = calculateCardMatches cards
            points 0 = 0
            points x = 2^(matchingCards - 1)

calculateCardMatches :: Card -> Int
calculateCardMatches (winningCards, myCards) = length $ intersect winningCards myCards

parseCard :: String -> Card
parseCard line = tuplify2 $ map (map readInt) numberStrings
    where
        numberStrings = map words (splitBy '|' ((last . splitBy ':') line))
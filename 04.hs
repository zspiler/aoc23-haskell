import System.Environment (getArgs)
import Data.List (intersect)
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
part2 cards = sum $ map (calculateScratchCardCopies cards (calculateMatchesPerCard cards)) (indices cards)

calculateScratchCardCopies :: [Card] -> Map.Map Int Int -> Int ->  Int -- cardIndex, cardsList, pointsMap
calculateScratchCardCopies cards matchesPerCard cardIndex = result
    where 
        result = 1 + sum (map (calculateScratchCardCopies cards matchesPerCard) copiesIndexes)
        numOfCopies = fromJust (Map.lookup cardIndex matchesPerCard)
        copiesIndexes = take numOfCopies [(cardIndex+1)..(length cards - 1)]

calculateMatchesPerCard :: [Card] -> Map.Map Int Int 
calculateMatchesPerCard cards = Map.fromList $ zip [0..] (map calculateCardMatches cards)

calculateCardPoints :: Card -> Int
calculateCardPoints cards@(winningCards, myCards) = points matchingCards
    where   matchingCards = calculateCardMatches cards
            points 0 = 0
            points x = 2^(matchingCards - 1)

calculateCardMatches :: Card -> Int
calculateCardMatches (winningCards, myCards) = length $ intersect winningCards myCards

parseCard :: String -> Card
parseCard line = tuplify2 (readInts (map splitNumbers (seperateNumbersGroups (last (removePrefix line)))))
    where
            readInts = map (map readInt) 
            splitNumbers = filter (not . null) . map removeSpaces . splitBy ' '
            seperateNumbersGroups = splitBy '|'
            removePrefix = splitBy ':'
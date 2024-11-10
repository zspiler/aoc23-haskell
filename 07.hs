import System.Environment (getArgs)
import Data.List (sortBy, sort, nub, groupBy, group)
import Utils (readInt)

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)

data Card = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Eq, Ord)

-- for debugging
instance Show Card where
    show A = "A"
    show K = "K"
    show Q = "Q"
    show J = "J"
    show T = "T"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"

type Hand = [Card]

type Pair = (Hand, Int) -- hand,  bid

main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let pairs = parsePairs $ lines contents

    let sortedPairs = sortBy (\(hand1,_) (hand2,_) -> compareHands hand2 hand1) pairs
    print $ part1 sortedPairs

part1 :: [Pair] -> Int
part1 pairs = sum $ zipWith (\rank (_, bid) -> rank * bid) [1..] pairs

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2
    | handType1 /= handType2 = compare handType1 handType2
    | otherwise = compare hand1 hand2
    where   handType1 = handType hand1
            handType2 = handType hand2

handType :: Hand -> HandType
handType hand
    | length (cardCounts hand) == 1 = FiveOfAKind
    | cardCounts hand == [1, 4] = FourOfAKind
    | cardCounts hand == [2, 3] = FullHouse
    | cardCounts hand == [1,1,3] = ThreeOfAKind
    | cardCounts hand == [1,2,2] = TwoPair
    | cardCounts hand == [1,1,1,2] = OnePair
    | length (cardCounts hand) == 5 = HighCard
    | otherwise = error "🤨"
    where cardCounts hand = sort $ map length $ group $ sort hand

parsePairs :: [String] -> [Pair]
parsePairs = map (\line -> (parseHand line, parseBid line))
    where   parseHand line = map parseCard (head . words $ line) 
            parseBid line = readInt $ words line !! 1

parseCard :: Char -> Card
parseCard c = case c of
    'A' -> A
    'K' -> K
    'Q' -> Q
    'J' -> J
    'T' -> T
    '9' -> Nine
    '8' -> Eight
    '7' -> Seven
    '6' -> Six
    '5' -> Five
    '4' -> Four
    '3' -> Three
    '2' -> Two
    _ -> error "🤨"
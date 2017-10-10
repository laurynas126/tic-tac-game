module Parser where

import Data.Either
import Data.List

data MoveData = MoveData {
    moveC :: (Int, Int),
    moveID :: String,
    moveV :: Char
} deriving (Show, Eq)

legalMoves :: [(Int, Int)]
legalMoves = [(1,1),(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]

test :: MoveData
test = MoveData (0,1) "abc" '4'

nullCoordinates :: (Int, Int)
nullCoordinates = (-1,-1)

newMove :: MoveData
newMove = MoveData nullCoordinates "" '\\'

msg1 :: String
msg1 = "di42ei0ei777ee"

msg2 :: String
msg2 = "d2:idi16ee"

msg3 :: String
msg3 = "d2:id5:MY_ID1:cli1ei2eee"

msg4 :: String
msg4 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:zee"

msg5 :: String
msg5 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:z4:prevd2:id5:UR_ID1:cli1ei1ee1:v1:Geee"

message1 :: String
message1 = "d1:cli1ei1ee1:v1:o2:id7:ufbqJdW4:prevd1:cli0ei2ee1:v1:x2:id11:OvEdLQavGnz4:prevd1:cli0ei0ee1:v1:x2:id7:ufbqJdWeee"

message2 :: String
message2 = "d1:cli2ei0ee2:id2:xO4:prevd1:cli0ei0ee2:id2:xO1:v1:oe1:v1:xe"

message3 :: String
message3 = "d1:cli0ei2ee2:id1:w4:prevd1:cli2ei1ee2:id5:WgEYE4:prevd1:cli1ei1ee2:id1:w1:v1:xe1:v1:oe1:v1:xe"

message4 :: String
message4 = "d1:cli2ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli1ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli2ei2ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli2ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli0ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli1ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

parseInt :: String -> (Int, String)
parseInt [] = (-1,[])
parseInt (num:rest) 
    | num == 'i' && 'e' `elem` rest && null [x | x <- takeWhile(/= 'e') rest, x `notElem` ['0'..'9']] =
        let
            iAsStr = takeWhile(/= 'e') rest
            strLength = length iAsStr + 1
            rest1 = drop strLength rest
            in (read iAsStr, rest1)
    | otherwise = (-99,rest)

move :: String -> Either String (Maybe (Int, Int, Char))
move msg = 
    let 
        validateResult = validateGame (parseDict msg)
        value 
            | isLeft validateResult = Left $ fromLeft  "Could not validate" validateResult
            | otherwise = Right (nextMove (fromRight  [] validateResult))
        in value

validateGame :: Either String [MoveData] -> Either String [MoveData]
validateGame (Left a) = Left a
validateGame (Right []) = Right []
validateGame (Right b) = 
    let
        allMoveList = [moveC x | x <- b]
        illegalMoves = [x | x <- allMoveList, x `notElem` legalMoves]
        duplicateMoves = allMoveList \\ nub allMoveList
        result
            | not (null illegalMoves) = Left $ "Illegal moves: " ++ show illegalMoves
            | not (null duplicateMoves) = Left $ "Duplicate moves: " ++ show duplicateMoves
            | otherwise = Right b
        in result
       
nextMove :: [MoveData] -> Maybe (Int, Int, Char)
nextMove [] = Just (1,1,'x')
nextMove moveList
    | length moveList == 9 = Nothing
    | otherwise = 
        let 
            allMoveList = [moveC x | x <- moveList]
            emptyList = [x | x <- legalMoves, x `notElem` allMoveList]
            firstEmpty = head emptyList
            result = (fst firstEmpty, snd firstEmpty, 'o')
        in Just result

parseDict :: String -> Either String [MoveData]
parseDict "de" = Right []
parseDict ('d':t) = parseDict' t [newMove] 0
    where
        parseDict' :: String -> [MoveData] -> Int -> Either String [MoveData]
        parseDict' [] acc _
            | null acc = Right []
            | head acc == newMove = Right []
            | otherwise = Right acc
        parseDict' (symbol:rest) moveList depth
            | depth == -99 = Left "Invalid data format: unexpected termination of dictionary/list: "
            | symbol == 'd' || symbol == 'l' = parseDict' rest moveList depth
            | symbol == 'e' = parseDict' rest moveList (depth + 1)
            | symbol `elem` ['0'..'9'] && notElem ':' rest 
                = Left "Invalid data format: number not terminated with ':'"
            | symbol `elem` ['0'..'9'] =
            let
                index
                    | length moveList > depth = depth
                    | otherwise = -99
                (fstPart, sndPart) = splitAt index moveList
                move1 = moveList !! index
                (value, remainder) = parseValue (symbol:rest)
                (move2, remainder2)
                    | value == "id" = parseID move1 remainder
                    | value == "c" = parseCoordinates move1 remainder
                    | value == "prev" = (newMove:[move1], remainder)
                    | value == "v" = parseV move1 remainder
                    | otherwise = ([move1], value)
                in parseDict' remainder2 (fstPart ++ move2 ++ tail sndPart) index
            | otherwise = Left ("Invalid data format: symbol " ++ [symbol] ++ " found")
parseDict _ = Left "Invalid data: Dictionary expected"

parseValue :: String -> (String, String)
parseValue [] = ([], [])
parseValue str@(first:xs)
    | first `elem` ['0'..'9'] && ':' `elem` str && null [x | x <- takeWhile(/= ':') str, x `notElem` ['0'..'9']] =
    let
        iAsStr = takeWhile(/= ':') str
        strLength = length iAsStr + 1
        number = read iAsStr :: Int
        value = take number $ drop strLength str
        remainder = drop (strLength + number) str
    in (value, remainder)
    | otherwise = ([first], xs)

parseID :: MoveData -> String -> ([MoveData], String)
parseID move1 [] = ([move1], [])
parseID move1 str =
    let
        (value, remainder) = parseValue str
    in ([move1 {moveID = value}], remainder)

parseCoordinates :: MoveData -> String -> ([MoveData], String)
parseCoordinates m [] = ([m], [])
parseCoordinates m ('l':'e':rest) = ([m], rest)
parseCoordinates m ('l':rest) = 
    let 
        (x, rem1) = parseInt rest
        (y, rem2) = parseInt rem1
        terminator = take 1 rem2
        rem3 
            | terminator == "e" = drop 1 rem2
            | otherwise = rem2
        in ([m {moveC = (x, y)}], rem3)
parseCoordinates m str = ([m], str)

parseV :: MoveData -> String -> ([MoveData], String)
parseV move1 [] = ([move1], [])
parseV move1 str =
    let
        (value, remainder) = parseValue str
        moveSymbol
            | null value = '\\'
            | length value > 1 = '\\'
            | otherwise = head value
    in ([move1 {moveV = moveSymbol}], remainder)
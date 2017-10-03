module Parser where

data MoveData = MoveData {
    moveC :: (Int, Int),
    moveID :: String,
    moveV :: Char
} deriving Show

test :: MoveData
test = MoveData (0,1) "abc" '4'

empty :: String
empty = "de"

msg :: String
msg = "di42ei0ei777ee"

msg2 :: String
msg2 = "d2:idi16ee"

msg3 :: String
msg3 = "d2:id5:MY_ID1:cli1ei2eee"

msg4 :: String
msg4 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:ze"

msg5 :: String
msg5 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:z4:prevd2:id5:UR_ID1:cli1ei1ee1:v1:Geee"


message :: String
message = "d1:cli1ei1ee1:v1:o2:id7:ufbqJdW4:prevd1:cli0ei2ee1:v1:x2:id11:OvEdLQavGnz4:prevd1:cli0ei0ee1:v1:x2:id7:ufbqJdWeee"

message2 :: String
message2 = "d1:cli2ei0ee2:id2:xO4:prevd1:cli0ei0ee2:id2:xO1:v1:oe1:v1:xe"

message3 :: String
message3 = "d1:cli0ei2ee2:id1:w4:prevd1:cli2ei1ee2:id5:WgEYE4:prevd1:cli1ei1ee2:id1:w1:v1:xe1:v1:oe1:v1:xe"

message4 :: String
message4 = "d1:cli2ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli1ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli2ei2ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli2ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli0ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli1ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"


parseInt :: String -> (Int, String)
parseInt (num:rest) 
    | num == 'i' =
        let
            iAsStr = takeWhile(/= 'e') rest
            strLength = length iAsStr + 1
            rest1 = drop strLength rest
            in (read iAsStr, rest1)
    | otherwise = error "No Integer"
parseInt [] = error "Empty list"

parseDict :: String -> ([MoveData], String)
parseDict "de" = ([], "")
parseDict ('d':t) = parseDict' t [MoveData (-1,-1) "" '\\'] 0
    where
        parseDict' :: String -> [MoveData] -> Int -> ([MoveData], String)
        parseDict' [] acc _ = (acc, "")
        parseDict' symbol [] _ = ([], symbol)
        parseDict' (symbol:rest) moveList@(_:acc) depth
            | symbol == 'd' || symbol == 'l' = parseDict' rest moveList (depth)
            | symbol == 'e' = parseDict' rest moveList (depth + 1)
            | symbol `elem` ['0'..'9'] =
            let
                (fstPart, sndPart) = splitAt depth moveList
                move = moveList !! depth
                (value, remainder) = parseValue (symbol:rest)
                (move2, remainder2)
                    | value == "id" = parseID move remainder
                    | value == "c" = parseCoordinates move remainder
                    | value == "prev" = (MoveData (-1,-1) "" '\\':[move], remainder)
                    | value == "v" = parseV move remainder
                    | otherwise = ([move], remainder)
                depth2
                    | value == "prev" = depth
                    | otherwise = depth

                -- in parseDict' remainder2 (move2 ++ acc) depth2
                in parseDict' remainder2 (fstPart ++ move2 ++ tail sndPart) depth2
            | otherwise = (moveList , rest)
parseDict _ = error "List expected"

parseValue :: String -> (String, String)
parseValue [] = ([], [])
parseValue str =
    let
        iAsStr = takeWhile(/= ':') str
        strLength = length iAsStr + 1
        number = read iAsStr :: Int
        value = take number $ drop strLength str
        remainder = drop (strLength + number) str
    in (value, remainder)

parseID :: MoveData -> String -> ([MoveData], String)
parseID move [] = ([move], [])
parseID move str =
    let
        (value, remainder) = parseValue str
    in ([move {moveID = value}], remainder)

parseCoordinates :: MoveData -> String -> ([MoveData], String)
parseCoordinates m [] = ([m], [])
parseCoordinates m "le" = ([m], [])
parseCoordinates m ('l':rest) = 
    let 
        (x, rem1) = parseInt rest
        (y, rem2) = parseInt rem1
        in ([m {moveC = (x, y)}], drop 1 rem2)
parseCoordinates _ _ = error "Incorrect coordinate format"

parseV :: MoveData -> String -> ([MoveData], String)
parseV move [] = ([move], [])
parseV move str =
    let
        (value, remainder) = parseValue str
    in ([move {moveV = head value}], remainder)
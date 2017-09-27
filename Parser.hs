module Parser where

data MoveData = MoveData {
    moveC :: (Int, Int),
    moveID :: String,
    moveV :: Char
} deriving Show

empty :: String
empty = "de"

msg :: String
msg = "di42ei0ei777ee"

msg2 :: String
msg2 = "d12:i16ee"

message :: String
message = "d1:cli1ei1ee1:v1:x2:id7:ufbqJdW4:prevl1:cli0ei2ee1:v1:x2:id11:OvEdLQavGnz4:prevl1:cli0ei0ee1:v1:x2:id7:ufbqJdWeee"

message2 :: String
message2 = "d1:cli2ei0ee2:id2:xO4:prevd1:cli0ei0ee2:id2:xO1:v1:oe1:v1:xe"

parseInt :: String -> (Int, String)
parseInt (num:rest) 
    | num == 'i' =
        let
            iAsStr = takeWhile(/= 'e') rest
            strLength = length iAsStr + 1
            rest1 = drop strLength rest
            in (read iAsStr, rest1)
    | num `elem` ['0'..'9'] =
        let
            iAsStr = num : takeWhile(/= ':') rest
            strLength = length iAsStr
            rest1 = drop strLength rest
            in (read iAsStr, rest1)
    | otherwise = error "No Integer"
parseInt [] = error "Empty list"

parseList :: String -> ([Int], String)
parseList "de" = ([], "")
parseList ('d':t) = parseList' t []
    where
        parseList' :: String -> [Int] -> ([Int], String)
        parseList' [] acc= (acc, "")
        parseList' (symbol:rest) acc 
            | symbol == 'l' = parseList' rest acc
            | symbol == 'e' = parseList' rest acc
            | symbol == 'i' =
            let
                (r, rest1) = parseInt (symbol:rest)
                in parseList' rest1 (r:acc)
            | symbol `elem` ['0'..'9'] =
            let
                (r, rest1) = parseString (symbol:rest)
                in parseList' rest1 acc
            | otherwise = (acc, rest)
parseList _ = error "List expected"

parseString :: String -> (String, String)
parseString (num:rest)
    | num `elem` ['0'..'9'] =
        let
            iAsStr = num : takeWhile(/= ':') rest
            strLength = length iAsStr
            number = read iAsStr :: Int
            value = take number $ drop strLength rest
            remainder = drop (strLength + number) rest
            in (value, remainder)
    | otherwise = error "No Integer"
parseString [] = error "Empty list"
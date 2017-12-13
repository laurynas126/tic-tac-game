{-# LANGUAGE OverloadedStrings #-}
module Parser where
    
    import Data.Either
    import Data.List
    import Data.Maybe
    import Control.Monad
    import Data.Function (on)

    data MoveData = MoveData {
        moveC :: (Int, Int),
        moveID :: String,
        moveV :: Char
    } deriving (Show, Eq)
    
    legalMoves :: [(Int, Int)]
    legalMoves = [(1,1),(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]

    allLines :: [(Int, Int, Int)]
    allLines = [(3,4,5), (1,4,7), (2,4,6), (0,4,8), (0,1,2),  (6,7,8), (0, 3, 6),  (2,5,8)]
    
    toSingleCoordinate :: (Int, Int) -> Int
    toSingleCoordinate (x, y) = x + 3*y

    toDoubleCoordinate :: Int -> (Int, Int)
    toDoubleCoordinate xy = (xy - 3 * floor (fromIntegral xy / 3), floor (fromIntegral xy / 3))

    test :: MoveData
    test = MoveData (0,1) "abc" '4'
    
    nullCoordinates :: (Int, Int)
    nullCoordinates = (-1,-1)
    
    newMove :: MoveData
    newMove = MoveData nullCoordinates "" '?'
    
    defaultUserID :: String
    defaultUserID = "LD"

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
    
    makeMove :: String -> (Bool, String)
    makeMove "" = (False, toBencode "" (snd (getNextMove [])))
    makeMove bencodeData = do
        let moves = parseDict bencodeData
        let validateResult = validateGame moves
        let (isDone, moveD) = getNextMove (fromRight  [] validateResult)
        let resultStr
                | isLeft validateResult = (True, fromLeft  "Could not validate" validateResult)
                | otherwise = (isDone, toBencode bencodeData moveD)
        resultStr

    getNextMove :: [MoveData] -> (Bool, MoveData)
    getNextMove [] = (False, MoveData (1,1) defaultUserID 'x')
    getNextMove moveList = do
        let allMoveList = [moveC x | x <- moveList]
        let emptyList = [(toSingleCoordinate x, '?') | x <- legalMoves, x `notElem` allMoveList]
        let coordList = sortMoves ([(toSingleCoordinate (moveC x),moveV x) | x <- moveList] ++ emptyList)
        let hasWinner = checkWinner coordList
        let winMove = getWinMove coordList
        let defMove = getDefMove coordList
        let def2Move = getDef2Move coordList
        let getMove
                | hasWinner || null emptyList = (True, newMove)
                | not (null winMove) = (True, toMoveData (head winMove))
                | not (null defMove) = (False, toMoveData (head defMove))
                | not (null def2Move) = (False, toMoveData (head def2Move))
                | otherwise = (False, MoveData (toDoubleCoordinate (fst (head emptyList))) defaultUserID 'x')
        if length emptyList == 1 
            then (True, snd getMove)
            else getMove

    toMoveData :: (Int, Char) -> MoveData
    toMoveData (n, c) = MoveData (toDoubleCoordinate n) defaultUserID c

    sortMoves :: Ord a => [(a, b)] -> [(a, b)]
    sortMoves = sortBy (compare `on` fst)

    toBencode :: String -> MoveData -> String
    toBencode "" md = "d1:cli" ++ show (fst (moveC md)) ++ "ei" ++ show (snd (moveC md)) ++ "ee2:id" ++  show (length (moveID md)) ++ ":" ++ moveID md ++ "1:v1:" ++ [moveV md] ++ "e"
    toBencode game md = "d1:cli" ++ show (fst (moveC md)) ++ "ei" ++ show (snd (moveC md)) ++ "ee2:id" ++  show (length (moveID md)) ++ ":" ++ moveID md ++ "4:prev" ++ game ++ "1:v1:" ++ [moveV md] ++ "e"

    checkWinner :: [(Int, Char)] -> Bool
    checkWinner moves = do
        let getWinLines = [al| al <- allLines, snd (moves !! fst3 al) == snd (moves !! snd3 al),snd (moves !! snd3 al) == snd (moves !! thd3 al), snd (moves !! fst3 al) /= '?']
        not (null getWinLines)

    getWinMove :: [(Int, Char)] -> [(Int, Char)]
    getWinMove moves = do
        al <- allLines
        let winMove
                | snd (moves !! fst3 al) == snd (moves !! snd3 al) && snd (moves !! fst3 al) /= '?' && snd (moves !! thd3 al) == '?' = [(thd3 al, snd (moves !! fst3 al))]
                | snd (moves !! fst3 al) == snd (moves !! thd3 al) && snd (moves !! fst3 al) /= '?' && snd (moves !! snd3 al) == '?' = [(snd3 al, snd (moves !! fst3 al))]
                | snd (moves !! snd3 al) == snd (moves !! thd3 al) && snd (moves !! snd3 al) /= '?' && snd (moves !! fst3 al) == '?' = [(fst3 al, snd (moves !! snd3 al))]
                | otherwise = []
        winMove
        
    getDefMove :: [(Int, Char)] -> [(Int, Char)]
    getDefMove moves = do
        al <- allLines        
        let defMove
                | snd (moves !! fst3 al) /= snd (moves !! snd3 al) && snd (moves !! fst3 al) /= '?' && snd (moves !! snd3 al) /= '?' && snd (moves !! thd3 al) == '?' = [(thd3 al, snd (moves !! fst3 al))]
                | snd (moves !! snd3 al) /= snd (moves !! thd3 al) && snd (moves !! snd3 al) /= '?' && snd (moves !! thd3 al) /= '?' && snd (moves !! fst3 al) == '?' = [(fst3 al, snd (moves !! thd3 al))]
                | snd (moves !! fst3 al) /= snd (moves !! thd3 al) && snd (moves !! fst3 al) /= '?' && snd (moves !! thd3 al) /= '?' && snd (moves !! snd3 al) == '?' = [(snd3 al, snd (moves !! fst3 al))]
                | otherwise = []
        defMove
        
    getDef2Move :: [(Int, Char)] -> [(Int, Char)]
    getDef2Move moves = do
        al <- allLines        
        let defMove
                | snd (moves !! snd3 al) /= '?' && snd (moves !! thd3 al) == '?' = [(thd3 al, invertMove (snd (moves !! snd3 al)))]
                | snd (moves !! fst3 al) /= '?' && snd (moves !! thd3 al) == '?' = [(thd3 al, invertMove (snd (moves !! fst3 al)))]
                | snd (moves !! thd3 al) /= '?' && snd (moves !! snd3 al) == '?' = [(snd3 al, invertMove (snd (moves !! thd3 al)))]
                | snd (moves !! thd3 al) /= '?' && snd (moves !! fst3 al) == '?' = [(fst3 al, invertMove (snd (moves !! thd3 al)))]
                | otherwise = []
        defMove
    invertMove :: Char -> Char
    invertMove 'x' = 'o'
    invertMove 'o' = 'x'
    invertMove _ = '?'

    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    snd3 :: (a, b, c) -> b
    snd3 (_, x, _) = x

    thd3 :: (a, b, c) -> c
    thd3 (_, _, x) = x

    getLastMove :: String -> MoveData
    getLastMove str = do
        let parse = parseDict str
        case parse of
            Right m -> last m
            _ -> newMove

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
                | symbol `elem` ['0'..'9'] && notElem ':' rest = Left "Invalid data format: number not terminated with ':'"
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
    parseDict etc = Left ("Invalid data: Dictionary expected: " ++ etc)
    
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

    isValidBencode :: String -> Bool
    isValidBencode ('d':xs) = last xs == 'e'
    isValidBencode _ = False
         
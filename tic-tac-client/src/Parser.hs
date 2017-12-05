module Parser where
    
    import Data.Either
    import Data.List
    import Data.Maybe
    import Control.Monad
    import Data.Function (on)

    g1 :: String
    g1 = "d1:cli1ei1ee2:id2:LD1:v1:xe"
      
    g2 :: String
    g2 = "d1:cli0ei0ee2:id2:LD4:prevd1:cli1ei1ee2:id2:LD1:v1:xe1:v1:oe"

    g3 :: String
    g3 = "d1:cli0ei1ee2:id2:LD4:prevd1:cli1ei1ee2:id2:LD1:v1:xe1:v1:oe"

    msg1 :: String
    msg1 = "d2:id5:MY_ID1:cli1ei2eee"
    
    msg2 :: String
    msg2 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:zee"
    
    msg3 :: String
    msg3 = "d2:id5:MY_ID1:cli1ei2ee1:v1:o4:prevd2:id5:UR_ID1:cli2ei0ee1:v1:z4:prevd2:id5:UR_ID1:cli1ei1ee1:v1:Geee"
    
    message1 :: String
    message1 = "d1:cli1ei1ee1:v1:o2:id7:ufbqJdW4:prevd1:cli0ei2ee1:v1:x2:id11:OvEdLQavGnz4:prevd1:cli0ei0ee1:v1:x2:id7:ufbqJdWeee"
    
    message2 :: String
    message2 = "d1:cli2ei0ee2:id2:xO4:prevd1:cli0ei0ee2:id2:xO1:v1:oe1:v1:xe"
    
    message3 :: String
    message3 = "d1:cli0ei2ee2:id1:w4:prevd1:cli2ei1ee2:id5:WgEYE4:prevd1:cli1ei1ee2:id1:w1:v1:xe1:v1:oe1:v1:xe"
    
    message4 :: String
    message4 = "d1:cli2ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli1ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli2ei2ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli2ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli0ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli1ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"
    
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
        let validateResult = validateGame (moves)
        let nMove = getNextMove (fromRight  [] validateResult)
        let resultStr
                | isLeft validateResult = (True, fromLeft  "Could not validate" validateResult)
                | otherwise = (fst nMove, toBencode bencodeData (snd nMove))
        resultStr

    getNextMove :: [MoveData] -> (Bool, MoveData)
    getNextMove [] = (False, MoveData (1,1) "LD" 'x')
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
                | otherwise = (False, MoveData (toDoubleCoordinate (fst (head emptyList))) "LD" 'x')
        if length emptyList == 1 
            then (True, snd getMove)
            else getMove

    toMoveData :: (Int, Char) -> MoveData
    toMoveData (n, c) = MoveData (toDoubleCoordinate n) "LD" c

    sortMoves :: Ord a => [(a, b)] -> [(a, b)]
    sortMoves = sortBy (compare `on` fst)

    toBencode :: String -> MoveData -> String
    toBencode "" md = "d1:cli" ++ show (fst (moveC md)) ++ "ei" ++ show (snd (moveC md)) ++ "ee2:id" ++  show (length (moveID md)) ++ ":" ++ moveID md ++ "1:v1:" ++ [moveV md] ++ "e"
    toBencode game md = "d1:cli" ++ show (fst (moveC md)) ++ "ei" ++ show (snd (moveC md)) ++ "ee2:id" ++  show (length (moveID md)) ++ ":" ++ moveID md ++ "4:prev" ++ game ++ "1:v1:" ++ [moveV md] ++ "e"

    list :: [(Int, Char)]
    list = [(0,'x'),(1,'?'),(2,'x'),(3,'?'),(4,'?'),(5,'?'),(6,'?'),(7,'?'),(8,'?')]

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
         
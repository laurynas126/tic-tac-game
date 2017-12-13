module GameClient where 

import HttpClient
import Parser

url :: String
url = "http://localhost:8080/"

runCycle :: String -> String -> IO ()
runCycle gameID playerID = do
    let gameUrl = url ++ gameID
    _ <- first playerID gameUrl
    runCycle' gameUrl 1

runCycle' :: String -> Int -> IO ()
runCycle' _ 500 = return ()
runCycle' _ 400 = return ()
runCycle' gameUrl status = do
    (responseCode, bencode) <- HttpClient.getHttp gameUrl
    let (isDone, nextMove)
            | responseCode /= 500 = Parser.makeMove bencode
            | otherwise = (True, "No response")
    (response2, result) <- if Parser.isValidBencode nextMove
        then HttpClient.postHttp gameUrl nextMove
        else do
            putStrLn nextMove
            return (500, "")
    if isDone
        then do
            putStrLn "Game is done"
            return ()
        else runCycle' gameUrl response2


first :: String -> String  -> IO (Int,String)
first "1" gameUrl = do
    let (_, move) = Parser.makeMove ""
    HttpClient.postHttp gameUrl move
first _ _ = return (500, "")

getStr :: [MoveData] -> String
getStr xs = concat [show (toSingleCoordinate (moveC c)) ++ " " ++ [moveV c] ++ "\n"| c <- xs]
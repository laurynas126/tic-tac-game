module GameClient where 

import HttpClient
import Parser

url :: String
url = "http://localhost:8080/"

runCycle :: String -> IO ()
runCycle gameID = do
    let gameUrl = url ++ gameID
    let (_, move) = Parser.makeMove ""
    runCycle' gameUrl move

runCycle' :: String -> String -> IO ()
runCycle' gameUrl newMove = do
    putStrLn ("Move       : " ++ show (Parser.getLastMove newMove))    
    (responseCode, responseBody) <- HttpClient.postHttp gameUrl newMove
    let serverMove = Parser.getLastMove responseBody
    _ <- if Parser.isValidBencode responseBody
        then putStrLn ("Server Move: " ++ show (Parser.getLastMove responseBody))  
        else putStrLn ("Server     : " ++ responseBody)
    let (isDone, nextMove)
            | responseCode /= 500 = Parser.makeMove responseBody
            | otherwise = (True, "No response")
    let isValid 
            | Parser.isValidBencode nextMove = True
            | otherwise = False
    if not isValid
        then do
            putStrLn "Game is done"
            return ()
        else runCycle' gameUrl nextMove

first :: String -> String  -> IO (Int,String)
first "1" gameUrl = do
    let (_, move) = Parser.makeMove ""
    HttpClient.postHttp gameUrl move
first _ _ = return (500, "")

getStr :: [MoveData] -> String
getStr xs = concat [show (toSingleCoordinate (moveC c)) ++ " " ++ [moveV c] ++ "\n"| c <- xs]
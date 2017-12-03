module GameClient where 

import System.Environment
import HttpClient
import Parser

url1 :: String
url1 = "http://tictactoe.haskell.lt/game/" ++ gameId ++ "/player/" ++ show playerId

gameId :: String
gameId = "holy shit!"

playerId :: Int
playerId = 2

runCycle :: IO ()
runCycle = do
    args <- getArgs
    let id
            | length args == 1 && ((last args == "1") || (last args == "2")) = last args
            | otherwise = error "Valid player ID: 1, 2 (use: tic-tac-client-exe <playerID>)"
    let url = "http://tictactoe.haskell.lt/game/" ++ gameId ++ "/player/" ++ id
    _ <- first id url
    bencode <- HttpClient.getHttp url
    print bencode
    let nextMove = Parser.makeMove bencode
    result <- HttpClient.postHttp url nextMove
    putStrLn nextMove
    putStrLn result

first :: String -> String  -> IO String
first "1" url = HttpClient.postHttp url (Parser.makeMove "")
first _ _ = return ""

getStr :: [MoveData] -> String
getStr xs = concat [show (toSingleCoordinate (moveC c)) ++ " " ++ [moveV c] ++ "\n"| c <- xs]
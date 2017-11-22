module GameClient where 

import HttpClient
import Parser

url :: String
url = "http://tictactoe.haskell.lt/game/" ++ gameId ++ "/player/" ++ show playerId

gameId :: String
gameId = "aa11"

playerId :: Int
playerId = 2

runCycle :: IO ()
runCycle = do
    bencode <- HttpClient.getHttp url
    print bencode
    let nextMove = Parser.makeMove bencode
    result <- HttpClient.postHttp url nextMove
    putStrLn nextMove
    putStrLn result

getStr :: [MoveData] -> String
getStr xs = concat [show (toSingleCoordinate (moveC c)) ++ " " ++ [moveV c] ++ "\n"| c <- xs]
module Main where

import System.Environment
import GameClient

url :: String
url = "http://tictactoe.haskell.lt/game/"

gameId :: String
gameId = "holy shit!"

main :: IO ()
main = do 
    args <- getArgs
    let (gameID, playerID)
            | length args == 2 && ((last args == "1") || (last args == "2")) = (head args, last args)
            | otherwise = (head args, "?") 
    if playerID == "?"
        then do 
            putStrLn "No valid parameters found! "
            putStrLn "      Valid player ID: 1, 2 "
            putStrLn "      (usage: tic-tac-client-exe <playerID> <gameID>)"
        else runCycle gameID playerID

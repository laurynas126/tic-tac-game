module Main where

import System.Environment
import GameClient

url :: String
url = "http://tictactoe.haskell.lt/game/"

main :: IO ()
main = do 
    args <- getArgs
    let gameID
            | length args == 1 = head args
            | otherwise = "?"
    if gameID == "?"
        then do 
            putStrLn "No valid parameters found! "
            putStrLn "      You need to specify gameID (can be chosen randomly)"
            putStrLn "      (usage: tic-tac-client-exe <gameID>)"
        else runCycle gameID

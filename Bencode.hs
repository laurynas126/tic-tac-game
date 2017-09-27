module Bencode where

num :: (Show a) => a -> String
num a = "You entered " ++ show a

list :: String -> [Integer]
list ('1':_) = [1]
list ('2':_) = [1]
list _ = []

numbers :: String -> [Int]
numbers x = [read [c] :: Int| c <- x, c `elem` ['0' .. '9']]

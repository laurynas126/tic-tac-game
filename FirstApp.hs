module FirstApp where
    
import Data.List

limit :: Int
limit = 100


func :: Int -> Int
func a
    | a < limit = func (a + a)
    | a > limit * 2 = a * a
    | otherwise = a

twoNums :: Int -> Int -> String
twoNums _ 1 = "Got 1"
twoNums _ 2 = "Got 2"
twoNums a b = show a ++ " " ++ show b

myDrop n xs = if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

first :: [a] -> a
first = head

getElement :: Int -> [a] -> a
getElement n xs = xs !! n

str :: String -> String
str a = a ++ a

deeper :: Int -> Int
deeper a
    | a < 10 = deeper (a + 1)
    | a == 15 = error "we are here"
    | otherwise = 15
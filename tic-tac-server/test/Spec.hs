import Test.QuickCheck
import Parser


coord :: Gen (Int, Int)
coord = (,) <$> choose (0,2) <*> choose (0,2)

value :: Gen Char
value = elements ['x','o']

valueToBencode :: String -> String
valueToBencode value = show (length value) ++ ":" ++ value

instance Arbitrary MoveData where
    arbitrary = MoveData <$> coord <*> arbitrary <*> value

prop_parser :: MoveData -> Bool
prop_parser md = getLastMove (toBencode "" md) == md

prop_parseInt :: Int -> String -> Bool
prop_parseInt number xs = fst (parseInt ("i" ++ show number ++ "e" ++ xs)) == number

prop_parseValue :: String -> Bool
prop_parseValue value = fst (parseValue (valueToBencode value)) == value

prop_sortMoves :: [(Int, Char)] -> Bool
prop_sortMoves [] = True
prop_sortMoves [a] = True
prop_sortMoves a = do
    let sorted = Parser.sortMoves a
    fst (head sorted) <= fst (last sorted)

main :: IO ()
main = do 
    quickCheck prop_parser
    quickCheck prop_sortMoves
    quickCheck (forAll (choose (0, maxBound)) prop_parseInt)
    quickCheck prop_parseValue
    putStrLn "++DONE++"

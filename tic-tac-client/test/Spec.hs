import Test.QuickCheck
import Parser

-- instance Arbitrary (Int, Int) where
--     arbitrary = elements [(1,1),(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]

instance Arbitrary Parser.MoveData where
        arbitrary = MoveData <$> arbitrary <*> arbitrary <*> arbitrary
      
prop_sortMoves :: [(Int, Char)] -> Bool
prop_sortMoves [] = True
prop_sortMoves [a] = True
prop_sortMoves a = do
    let sorted = Parser.sortMoves a
    head sorted <= last sorted
        
prop_toDoubleCoordinate :: Bool
prop_toDoubleCoordinate = do
    let r = choose (1,8) :: Int
    r == Parser.toSingleCoordinate (Parser.toDoubleCoordinate r)

main :: IO ()
main = do
    quickCheck prop_sortMoves
    quickCheck prop_toDoubleCoordinate
    putStrLn "+++ DONE"

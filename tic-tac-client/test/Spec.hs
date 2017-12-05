import Test.QuickCheck
import Test.HUnit
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
    fst (head sorted) <= fst (last sorted)
        
prop_toDoubleCoordinate :: Int -> Bool
prop_toDoubleCoordinate r = r == Parser.toSingleCoordinate (Parser.toDoubleCoordinate r)

parseDictRez :: Either String [MoveData]
parseDictRez = Parser.parseDict "d1:cli1ei1ee2:id2:LD1:v1:xe"

moveRez :: MoveData
moveRez = MoveData (1,1) "LD" 'x'

testBencodeParsing = TestCase (assertEqual "bencode -> moveData" parseDictRez (Right [moveRez]))

testFirstMove = TestCase (assertBool "first move" (take 11 (snd (Parser.makeMove "")) == "d1:cli1ei1e"))

boardArg1 :: [(Int, Char)]
boardArg1 = [(0,'x'),(1,'?'),(2,'x'),(3,'?'),(4,'?'),(5,'?'),(6,'?'),(7,'?'),(8,'?')]
testWinMove1 = TestCase(assertEqual "getWinMove [(1,'x')]" (Parser.getWinMove boardArg1) [(1,'x')])

boardArg2 :: [(Int, Char)]
boardArg2 = [(0,'x'),(1,'?'),(2,'?'),(3,'?'),(4,'?'),(5,'?'),(6,'?'),(7,'?'),(8,'?')]
testWinMove2 = TestCase(assertEqual "getWinMove []" (Parser.getWinMove boardArg2) [])

boardArg3 :: [(Int, Char)]
boardArg3 = [(0,'o'),(1,'?'),(2,'?'),(3,'?'),(4,'o'),(5,'?'),(6,'?'),(7,'?'),(8,'o')]
testWinner = TestCase(assertBool "checkWinner True" (Parser.checkWinner boardArg3))

main :: IO ()
main = do
    let tests = TestList [TestLabel "bencode -> moveData" testBencodeParsing, 
                TestLabel "first move" testFirstMove,
                TestLabel "win [(2,'x')]" testWinMove1,
                TestLabel "win []" testWinMove2,
                TestLabel "checkWinner True" testWinner]
    runTestTT tests
    quickCheck prop_sortMoves
    quickCheck prop_toDoubleCoordinate
    putStrLn "+++ DONE"

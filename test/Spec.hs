import AVL
import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )
import           Data.Maybe

main :: IO ()
main = runTestTTAndExit tests

insertElements :: Ord t => Tree t -> [t] -> Tree t
insertElements = foldl (flip insert)

testEmpty :: Test
testEmpty = TestCase (assertEqual "TestEmpty" (isEmpty empty) True)

testSize :: Test
testSize = TestCase (assertEqual "TestSize" (size (insertElements empty [1..10])) 10) 

testHeight :: Test
testHeight = TestCase (assertEqual "TestHeight" (height (insertElements empty [1..10])) 4) 

testFind :: Test
testFind = TestCase (assertEqual "TestFind" (fromJust(find 4 (insertElements empty [1 .. 10]))) 4) 

tests :: Test
tests = TestList [testEmpty, testSize, testHeight, testFind]
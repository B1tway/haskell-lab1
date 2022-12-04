{-# Language ScopedTypeVariables, GADTs #-}
import AVL
import Data.Maybe
import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )
import Test.QuickCheck as QC

main :: IO ()
main = runTestTTAndExit tests

insertElements :: Ord t => Tree t -> [t] -> Tree t
insertElements = foldl (flip insert)

testEmpty :: Test
testEmpty = TestCase (assertEqual "TestLeaf" (isLeaf empty) True)

testSize :: Test
testSize = TestCase (assertEqual "TestSize" (size (insertElements empty [1..10])) 10) 

testHeight :: Test
testHeight = TestCase (assertEqual "TestHeight" (height (insertElements empty [1..10])) 4) 

testFind :: Test
testFind = TestCase (assertEqual "TestFind" (fromJust(find 4 (insertElements empty [1 .. 10]))) 4) 

testDelete :: Test
testDelete = TestCase (assertEqual "TestDelete" (isNothing (find 4(delete 4(insertElements empty [1 .. 10])))) True)

tests :: Test
tests = TestList [testEmpty, testSize, testHeight, testFind, testDelete]
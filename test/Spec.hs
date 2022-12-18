
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
import           AVL
import           Data.Maybe            (fromJust, isNothing)
import           Test.Tasty
import           Test.Tasty.HUnit      as HU
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

insertElements :: Tree Int -> [Int] -> Tree Int
insertElements = foldl (flip insert)

testEmpty :: TestTree
testEmpty = testCase "TestLeaf" (assertEqual [] (isLeaf empty) True)

testSize :: TestTree
testSize = testCase "TestSize"  (assertEqual [] (size (insertElements empty [1..10])) 10)

testHeight :: TestTree
testHeight = testCase "TestHeight" (assertEqual  [] (height (insertElements empty [1..10])) 4)

testFind :: TestTree
testFind = testCase "TestFind" (assertEqual [] (fromJust(find 4 (insertElements empty [1 .. 10]))) 4)

testDelete :: TestTree
testDelete = testCase "TestDelete" (assertEqual [] (isNothing (find 4(delete 4(insertElements empty [1 .. 10])))) True)

testFilter :: TestTree
testFilter = testCase "TestFilter" (assertEqual [] (filterT (>4) tr ) expected_list ) where
                    expected_list = [5, 6]
                    tr = insertElements empty [6, 5, 4, 3, 2, 1]


instance Arbitrary (Tree Int) where
    arbitrary = sized $ \n ->
        return empty


qcTestInsert :: TestTree
qcTestInsert = QC.testProperty "property based test: insert => you can find it" (\(n :: Int, t :: Tree Int)  -> isElem n (insert n t))

tests :: TestTree
tests = testGroup "tests"
    [
    testGroup "HUnit tests" [testEmpty, testSize, testHeight, testFind, testDelete, testFilter],
    testGroup "QC tests" [qcTestInsert]
    ]

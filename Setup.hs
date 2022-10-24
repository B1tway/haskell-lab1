import Task10Lib (sumOfPrimesTail, sumOfPrimesModule, sumOfPrimesMap, sumOfPrimesFold, sumOfPrimesInfList)
import Task21Lib (sumOfAmicableFold, sumOfAmicableInfList, sumOfAmicableMap, sumOfAmicableModule, sumOfAmicableRec)
import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

main :: IO ()
main = runTestTTAndExit tests

test1_10 :: Test
test1_10  = TestCase (assertEqual "for (sumOfPrimesFold)," (sumOfPrimesFold 10000) 5736396)

test2_10 :: Test
test2_10 = TestCase (assertEqual "for (sumOfPrimesModule)," sumOfPrimesModule 5736396)

test3_10 :: Test
test3_10 = TestCase (assertEqual "for (sumOfPrimesTail)," (sumOfPrimesTail 10000) 5736396)

test4_10 :: Test
test4_10 = TestCase (assertEqual "for (sumOfPrimesMap)," (sumOfPrimesMap 10000) 5736396)

test5_10 :: Test
test5_10 = TestCase (assertEqual "for (sumOfPrimesMap)," (sumOfPrimesInfList 10000) 5736396)

test1_21 :: Test
test1_21  = TestCase (assertEqual "for (sumOfAmicableFold)," (sumOfAmicableFold 1000) 504)

test2_21 :: Test
test2_21 = TestCase (assertEqual "for (sumOfAmicableModule)," (sumOfAmicableModule 1000) 504)

test3_21 :: Test
test3_21 = TestCase (assertEqual "for (sumOfAmicableTail)," (sumOfAmicableRec 1000) 504)

test4_21 :: Test
test4_21 = TestCase (assertEqual "for (sumOfAmicableMap)," (sumOfAmicableMap 1000) 504)

test5_21 :: Test
test5_21 = TestCase (assertEqual "for (sumOfAmicableMap)," (sumOfAmicableInfList 1000) 504)

tests :: TestList
tests = TestList [TestLabel "test1_10" test1_10, TestLabel "test2_10" test2_10, TestLabel "test3_10" test3_10, TestLabel "test4_10" test4_10, TestLabel "test5_10" test5_10, TestLabel "test1_21" test1_21, TestLabel "test2_21" test2_21, TestLabel "test3_21" test3_21, TestLabel "test4_21" test4_21, TestLabel "test5_21" test5_21]

import Task10Lib (sumOfPrimesTail, sumOfPrimesModule, sumOfPrimesMap, sumOfPrimesFold, sumOfPrimesInfList)
import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

main :: IO ()
main = runTestTTAndExit tests

test1 :: Test
test1 = TestCase (assertEqual "for (sumOfPrimesFold)," (sumOfPrimesFold 10000) 5736396)

test2 :: Test
test2 = TestCase (assertEqual "for (sumOfPrimesModule)," sumOfPrimesModule 5736396)

test3 :: Test
test3 = TestCase (assertEqual "for (sumOfPrimesTail)," (sumOfPrimesTail 10000) 5736396)

test4 :: Test
test4 = TestCase (assertEqual "for (sumOfPrimesMap)," (sumOfPrimesMap 10000) 5736396)

test5 :: Test
test5 = TestCase (assertEqual "for (sumOfPrimesMap)," (sumOfPrimesInfList 10000) 5736396)


tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5]
module Task21Lib(sumOfAmicableMap, sumOfAmicableFold) where

sumOfAmicableMap :: Int -> Int
sumOfAmicableMap = sum . map checkAmicable . toVector . primeFactorsSumList

sumOfAmicableFold :: Int -> Int
sumOfAmicableFold = sum . map fst . filter checkAmicableBool . toVector . primeFactorsSumList

toVector:: [Int] -> [(Int, Int)]
toVector = zip [1..]

checkAmicable :: (Int, Int) -> Int
checkAmicable (index, factorSum) =
    if  factorSum > 0 && (factorSum /= index) && (sums!!(index - 1) == factorSum) && (sums!!(factorSum - 1) == index) then
        index
    else 0
    where sums = primeFactorsSumList 10000

checkAmicableBool :: (Int, Int) -> Bool
checkAmicableBool (index, factorSum) =
    factorSum > 0 && (factorSum /= index) && (sums!!(index - 1) == factorSum) && (sums!!(factorSum - 1) == index)
    where sums = primeFactorsSumList 10000

primeFactorsSumList :: Int -> [Int]
primeFactorsSumList = map primeFactorsSum . generateList

generateList :: Int -> [Int]
generateList x = [1..x]

primeFactorsSum :: Int -> Int
primeFactorsSum = sum . divisors

divisors:: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]
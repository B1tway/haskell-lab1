module Task10Lib(sumOfPrimesTail, sumOfPrimesModule, sumOfPrimesMap, sumOfPrimesFold, sumOfPrimesInfList) where

sumOfPrimesMap :: Int -> Int
sumOfPrimesMap = sum . map isPrimeN . generateList

sumOfPrimesFold :: Int -> Int
sumOfPrimesFold = sum . filter isPrime . generateList

sumOfPrimesInfList :: Int -> Int
sumOfPrimesInfList x = sum . filter isPrime . takeWhile (< x) $ [1..] 

sequencePrimes :: [Int]
sequencePrimes = [1..10000]

sumOfPrimesModule :: Int
sumOfPrimesModule = sum . filter isPrime $ sequencePrimes

sumOfPrimesTail:: Int -> Int
sumOfPrimesTail x
    | x == 1 = 0
    | isPrime x = x + sumPrimes
    | otherwise = sumPrimes
    where 
        sumPrimes = sumOfPrimesTail next
        next = x - 1

generateList :: Int -> [Int]
generateList x = [1..x] 

isPrimeN :: Int -> Int
isPrimeN x 
    | isPrime x = x
    | otherwise = 0

isPrime :: Integral a => a -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..k - 1], k `mod` x == 0]
module Approximation(linearApproximation, segmentApproximation, CalculatedPoints(Linear, a, b, xPoints, yPoints, Segment)) where

import Data.List
import Data.Maybe

calcLinearCoefficients :: Int -> [Double] -> [Double] -> [Double] -> [Double] -> (Double, Double)
calcLinearCoefficients length xList xxList yList xyList = 
    let 
        lengthDouble = fromIntegral length
        sx = sum xList
        sxx = sum xxList
        sy = sum yList
        sxy = sum xyList

        delta = sxx * lengthDouble - sx * sx
        delta1 = sxy * lengthDouble - sx * sy
        delta2 = sxx * sy - sx * sxy

        aC = delta1 / delta
        bC = delta2 / delta
    in
        (aC,bC)

    
data CalculatedPoints = 
    Linear {a :: Double, b :: Double, xPoints :: [Maybe Double], yPoints :: [Maybe Double] }
    | Segment {xPoints :: [Maybe Double], yPoints :: [Maybe Double]}
    deriving Show


calculatedX :: Double -> Double -> Double -> [Maybe Double]
calculatedX left right step = 
    let
        generated = [left, left + step .. right]
        output = map Just generated
    in
        output

toLower :: Double -> [Double] -> Maybe Double
toLower val [] = Nothing
toLower val (x:xs) = if val > x && isNothing (toLower val xs) then Just x else toLower val xs

toUpper :: Double -> [Double] -> Maybe Double
toUpper val [] = Nothing
toUpper val (x:xs) = if x >= val then Just x else toUpper val xs

linearApproximation :: Double -> Double -> Double -> Int -> [Double] -> [Double] -> CalculatedPoints
linearApproximation left right step length xList yList = 
    let
        xxList = map (\x -> x*x) xList
        xyList = zipWith (*) xList yList

        coefficients = calcLinearCoefficients length xList xxList yList xyList
        generatedX = calculatedX left right step
        generatedY = map (\mx -> mx >>= \x -> return $ fst coefficients * x + snd coefficients) generatedX
    in
        uncurry Linear coefficients generatedX generatedY
    
calcForSegment :: Double -> Double -> Double -> [Double] -> [Double] -> Maybe Double
calcForSegment xGiven xLower xUpper xList yList = 
    let
        yLower = yList !! fromJust (elemIndex xLower xList)
        yUpper = yList !! fromJust (elemIndex xUpper xList)

    in
        Just ((xGiven - xLower) * (yUpper - yLower) / (xUpper - xLower) + yLower)

segmentApproximation :: Double -> Double -> Double -> Int -> [Double] -> [Double] -> CalculatedPoints
segmentApproximation left right step length xList yList = 
    let
        generatedX = calculatedX left right step
        generatedY = map (\mx -> mx >>= \x -> toLower x xList >>= \y -> toUpper x xList >>= \z -> calcForSegment x y z xList yList) generatedX
    in 
        Segment generatedX generatedY
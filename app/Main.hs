{-#LANGUAGE ScopedTypeVariables#-}

module Main (main) where

import CLI (parseInput,prettyOutput, ProgramInput(method, left, right, step, count), InputPoints(xPoints, yPoints))
import Approximation(linearApproximation, segmentApproximation)

checkAndExec :: Char -> ProgramInput -> InputPoints -> IO()
checkAndExec args s t
  | 'l' == args = prettyOutput $ linearApproximation (left s) (right s) (step s) (count s) (xPoints t) (yPoints t)
  | 's' == args = prettyOutput $ segmentApproximation(left s) (right s) (step s) (count s) (xPoints t) (yPoints t)
  | otherwise = putStr ""
           
checkArgs :: [Char] -> ProgramInput -> InputPoints -> IO ()
checkArgs [] _ _ = putStrLn "Finished"
checkArgs (x:xs) input p = do
    checkAndExec x input p
    checkArgs xs input p

main :: IO()
main =  do
    s <- parseInput 
    let (input, points) = s
    checkArgs (method input) input points
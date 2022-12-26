{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module CLI (parseInput,  ProgramInput(ProgramInput,left, right, step, count, method),  InputPoints(InputPoints, xPoints, yPoints),prettyOutput) where

import           Approximation          (CalculatedPoints (Linear, Segment))
import           Control.Monad
import           Data.Maybe
import qualified Data.Text              as T (pack, splitOn, unpack)
import           System.Console.CmdArgs

data ProgramInput = ProgramInput {
    left :: Double, right :: Double,
    step :: Double, count :: Int,
    method :: String } deriving (Show, Data, Typeable)

pInput :: ProgramInput
pInput = ProgramInput {
    left = def &= help "Left border",
    right = def &= help "Right border",
    step = def &= help "Iteration step",
    count = def &= help "Number of points",
    method = def &= help "Approximation method"
}

data InputPoints = InputPoints {
    xPoints :: [Double], yPoints :: [Double] } deriving (Show)

parseInput :: IO (ProgramInput, InputPoints)
parseInput = do

    input <- cmdArgs pInput
    let (ProgramInput _ _ _ count' _) = input
    strPoints <- replicateM count' getLine
    let parsedPoints = map (T.splitOn ":" . T.pack ) strPoints

    let xPoints' = map (\x -> read (T.unpack $ head x) :: Double) parsedPoints
    let yPoints' = map (\x -> read (T.unpack $ last x) :: Double) parsedPoints
    let points = InputPoints xPoints' yPoints'
    return (input, points)


-- Output

prettyPoints :: [Maybe Double] -> [Maybe Double] -> IO()
prettyPoints [] _ = do
    putStrLn ""
prettyPoints xL yL = do
    putStr "x: "
    if isJust (head xL) then putStr $ show (fromJust (head xL)) else putStr "undefined"

    putStr " y: "
    maybe (print "undefined") print $ head yL
    prettyPoints  (tail xL) (tail yL)



prettyOutput :: CalculatedPoints -> IO ()
prettyOutput (Linear k x xP yP)  = do
    print $ "Result for approximated linear function y = " ++ show k ++ "*x + " ++ show x
    prettyPoints xP yP
prettyOutput (Segment xP yP) = do
    print  "Results for segment approximation"
    prettyPoints xP yP

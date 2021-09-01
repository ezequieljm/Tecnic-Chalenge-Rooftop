module Exercice1 where

import Data.Maybe (isNothing)
import Text.Read (readMaybe)

makeFigure :: Int -> IO ()
makeFigure numSize = mapM_ print $ makeMatrix numSize
    where
        makeMatrix :: Int -> [[Int]]
        makeMatrix size
            | even size = if checkNoFourEven size then createSpiralMatrix size matrixSixNoFour else createSpiralMatrix size matrixSix
            | otherwise = if checkNoFourOdd size then createSpiralMatrix size matrixFiveNoFour else createSpiralMatrix size matrixFiveFour
            where 
                createSpiralMatrix :: Int -> [[Int]] -> [[Int]]
                createSpiralMatrix dimension currentMatrix  
                    | length currentMatrix == dimension = currentMatrix 
                    | otherwise = createSpiralMatrix dimension (makeNewMatrix currentMatrix)
                makeNewMatrix :: [[Int]] -> [[Int]]
                makeNewMatrix spiralMatrix@(m:mx) = 
                    if head m == 0 then 
                        buildLayer spiralMatrix 0 1
                    else   
                        buildLayer spiralMatrix 1 0
                buildLayer :: [[Int]] -> Int -> Int -> [[Int]]
                buildLayer (m:mx) w z =
                    let newHead = [w] ++ m ++ [z]
                        newRestMx = map (\m -> [z] ++ m ++ [z]) mx
                        newList = newHead : newRestMx
                        lenMx = length newList + 2
                        repOne = repNum z lenMx
                        in [repOne] ++ newList ++ [repOne]
                checkNoFourOdd :: Int -> Bool
                checkNoFourOdd num  | num == 5  = True
                                    | num < 5   = False
                                    | otherwise = checkNoFourOdd (num - 4)
                checkNoFourEven :: Int -> Bool
                checkNoFourEven num | num == 6 = True
                                    | num < 6 = False
                                    | otherwise = checkNoFourEven (num - 4)
                -- Data matrix
                matrixFiveNoFour = [[1,1,1,1,1],[0,0,0,0,1],[1,1,1,0,1],[1,0,0,0,1],[1,1,1,1,1]]
                matrixFiveFour = [[0,0,0,0,0],[1,1,1,1,0],[0,0,0,1,0],[0,1,1,1,0],[0,0,0,0,0]]
                matrixSix = [[0,0,0,0,0,0],[1,1,1,1,1,0],[0,0,0,0,1,0],[0,1,0,0,1,0],[0,1,1,1,1,0],[0,0,0,0,0,0]]
                matrixSixNoFour = [[1,1,1,1,1,1],[0,0,0,0,0,1],[1,1,1,1,0,1],[1,0,0,1,0,1],[1,0,0,0,0,1],[1,1,1,1,1,1]]

repNum :: Int -> Int -> [Int]
repNum 1 lenMatrix = replicate lenMatrix 1
repNum 0 lenMatrix = replicate lenMatrix 0

controlMaybeInt :: String -> Maybe Int
controlMaybeInt = readMaybe

maybeLessThanFive :: Int -> Maybe Int
maybeLessThanFive x = if x < 5 then Nothing else Just x

checkValue :: String -> Maybe Int
checkValue inputUser = controlMaybeInt inputUser >>= maybeLessThanFive

controlInput :: Maybe Int -> IO Int
controlInput Nothing = do
    putStrLn "The value isn't valid. Pleas re-enter"
    newInput <- getLine
    controlInput $ checkValue newInput
controlInput (Just x) = return x

main :: IO ()
main = do
    putStrLn "Enter a number: "
    value       <- getLine
    validNumber <- controlInput $ checkValue value
    makeFigure validNumber

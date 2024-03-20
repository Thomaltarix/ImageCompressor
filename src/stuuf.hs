{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Main
-}

module Main (main) where

import System.Environment
import Data.Maybe
import GHC.Float

data Vector = Vector {x :: Int, y :: Int, z :: Int} deriving (Show)

getVector :: (Int, Int, Int) -> Maybe Vector
getVector (vx, vy, vz) = Just $ Vector vx vy vz

parseVector :: String -> Maybe Vector
parseVector str = getVector (read str :: (Int, Int, Int))

-- getDistance :: Vector -> Vector -> Double
-- getDistance (Vector x1 y1 z1) (Vector x2 y2 z2) = sqrt $ int2Double (x2 - x1) ** 2 + int2Double (y2 - y1) ** 2 + int2Double (z2 - z1) ** 2

joinMaybeList :: Maybe [a] -> Maybe [a] -> Maybe [a]
joinMaybeList Nothing _ = Nothing
joinMaybeList _ Nothing = Nothing
joinMaybeList (Just a) (Just b) = Just $ a ++ b

handleArgs :: [String] -> Maybe [Vector]
handleArgs [] = Nothing
handleArgs [a] = do
    case parseVector a of
        Just vector -> Just [fromJust $ Just vector]
        Nothing -> Nothing
handleArgs (a:xs) = joinMaybeList (handleArgs [a]) (handleArgs xs)

-- getClosestVector :: Vector -> [Vector] -> Vector
-- getClosestVector _ [] = error "Empty list"
-- getClosestVector _ [a] = a
-- getClosestVector v (a:xs)
--     | getDistance v a < getDistance v (getClosestVector v xs) = a
--     | otherwise = getClosestVector v xs

getSum :: Vector -> [Vector] -> Double
getSum _ [] = 0
getSum v (a:xs) = getDistance v a + getSum v xs

getSumList :: Int -> [Vector] -> [Double]
getSumList _ [] = []
getSumList 0 _ = []
getSumList n vectors = getSum (vectors !! (n - 1)) vectors : getSumList (n - 1) vectors

getMaxFromList :: [Double] -> Double
getMaxFromList [] = error "Empty list"
getMaxFromList [a] = a
getMaxFromList (a:xs)
    | a > getMaxFromList xs = a
    | otherwise = getMaxFromList xs

getIndexOfMax :: [Double] -> Int
getIndexOfMax [] = 0
getIndexOfMax [_] = 0
getIndexOfMax (a:xs)
    | abs (a - getMaxFromList (a:xs)) < 0.001 = 0
    | otherwise = 1 + getIndexOfMax xs

popFromArray :: Int -> [a] -> [a]
popFromArray _ [] = []
popFromArray 0 (_:xs) = xs
popFromArray n (a:xs) = a : popFromArray (n - 1) xs

removeFarestVectors :: Int -> [Vector] -> [Vector]
removeFarestVectors 0 vectors = vectors
removeFarestVectors _ [] = []
removeFarestVectors n vectors = removeFarestVectors (n - 1) (popFromArray (getIndexOfMax (reverse (getSumList (length vectors) vectors))) vectors)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then putStrLn "Invalid arguments"
    else do
        content <- readFile (args !! 0)
        let vectors = handleArgs $ words content
        let sheesh = removeFarestVectors 5 (fromJust vectors)
        print sheesh

{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- FileParsing
-}

module FileParsing (getFile,
                    fillConf,
                    Conf(..),
                    Vector(..)) where

import System.IO
import Control.Exception
import Data.Char
import Vector (Vector(..))

data Type = Coord | Color deriving (Show, Eq)

data Conf = Conf [(Vector, Vector)] deriving (Show)

getFile :: Maybe String -> IO (Maybe String)
getFile (Just path) = do
    file <- try $ openFile path ReadMode :: IO (Either SomeException Handle)
    case file of
        Left _ -> return Nothing
        Right file' -> do
            content <- hGetContents file'
            return $ Just content
getFile _ = return Nothing

addToVector :: Vector -> Double -> Type -> Vector
addToVector (Vector vec) value Coord
    | value >= 0 = Vector (value : vec)
    | otherwise = Vector []
addToVector (Vector vec) value Color
    | value >= 0 && value <= 255 = Vector (value : vec)
    | otherwise = Vector []

parseLine :: String -> String -> Type -> Vector
parseLine "" _ _ = Vector []
parseLine ('(':xs) "" valType = parseLine xs "" valType
parseLine ('(':xs) str valType =
    addToVector (parseLine xs "" valType) (read str :: Double) valType
parseLine (')':xs) "" valType = parseLine xs "" valType
parseLine (')':xs) str valType =
    addToVector (parseLine xs "" valType) (read str :: Double) valType
parseLine (',':xs) "" valType = parseLine xs "" valType
parseLine (',':xs) str valType =
    addToVector (parseLine xs "" valType) (read str :: Double) valType
parseLine (x:xs) str valType
    | isDigit x = parseLine xs (str ++ [x]) valType
    | otherwise = Vector []

parseFile :: [String] -> [(Vector, Vector)]
parseFile [] = []
parseFile (x1:x2:xs) =
    (parseLine x1 "" Coord, parseLine x2 "" Color) : parseFile xs
parseFile _ = []

getNbLines :: String -> Int
getNbLines "" = 0
getNbLines ('\n':xs) = 1 + getNbLines xs
getNbLines (_:xs) = getNbLines xs

checkVectorList :: [(Vector, Vector)] -> Bool
checkVectorList [] = True
checkVectorList ((Vector x, Vector y):xs)
    | length x /= 2 || length y /= 3 = False
    | otherwise = checkVectorList xs

fillConf :: String -> Maybe Conf
fillConf str = let vectorList = parseFile (words str) in
    if length vectorList == getNbLines str && checkVectorList vectorList
        then Just (Conf vectorList)
        else Nothing

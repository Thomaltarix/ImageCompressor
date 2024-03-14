{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- FileParsing
-}

module FileParsing (getFile,
                    fillConf,
                    Conf(..)) where

import System.IO
import Control.Exception
import Data.Char

data Vector = Vector [Double] deriving (Show)

data Type = Coord | Color deriving (Show, Eq)

data Conf = Conf [(Vector, Vector)] deriving (Show)

getFile :: Maybe String -> IO (Maybe String)
getFile (Just path) = do
    file <- try $ openFile path ReadMode :: IO (Either SomeException Handle)
    case file of
        Left _ -> return Nothing
        Right file -> do
            content <- hGetContents file
            return $ Just content
getFile _ = return Nothing

addToVector :: Vector -> Double -> Type -> Vector
addToVector (Vector vec) value valType
    | valType == Coord && isCoord value = Vector (value : vec)
    | valType == Color && isColor value = Vector (value : vec)
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
parseFile (x1:x2:xs) = (parseLine x1 "" Coord, parseLine x2 "" Color) : parseFile xs
parseFile _ = []
fillConf :: String -> Maybe Conf
fillConf str = do
    let lines' = words str
    let vectorList = parseFile lines'
    if length vectorList == getNbLines str && checkVectorList vectorList
        then Just (Conf vectorList)
        else Nothing

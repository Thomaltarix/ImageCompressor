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

fillConf :: String -> Maybe Conf
fillConf _ = Nothing

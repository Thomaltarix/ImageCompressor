{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Parsing
-}

module Parsing (Options(..),
                defaultOptions,
                getInt,
                getDouble,
                options,
                checkEmpty,
                parseArgs) where

import System.Console.GetOpt
import Text.Read
import Data.Maybe

data Options = Options {
    colorNb :: Maybe Int,
    convergence :: Maybe Double,
    filePath :: Maybe String
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    colorNb  = Nothing,
    convergence = Nothing,
    filePath = Nothing
}

getInt :: String -> Maybe Int
getInt str = readMaybe str :: Maybe Int

getDouble :: String -> Maybe Double
getDouble str = readMaybe str :: Maybe Double

options :: [OptDescr (Options -> Options)]
options =
    [Option ['n'] [""] (ReqArg (\arg opts -> opts
    {colorNb = getInt arg}) "") "Number of colors",
    Option ['l'] [""] (ReqArg (\arg opts -> opts
    {convergence = getDouble arg}) "") "The convergence limit",
    Option ['f'] [""] (ReqArg (\arg opts -> opts
    {filePath = Just arg}) "") "Path of the file"]

checkEmpty :: ([Options -> Options], [String], [String]) -> Maybe Options
checkEmpty (opts, [], []) = do
    let opt = Just (foldl (flip id) defaultOptions opts) in
        if  isNothing (colorNb (fromJust opt)) ||
            isNothing (convergence (fromJust opt)) ||
            isNothing (filePath (fromJust opt))
            then Nothing
            else opt
checkEmpty _ = Nothing

parseArgs :: [String] -> Maybe Options
parseArgs args = checkEmpty (getOpt Permute options args)

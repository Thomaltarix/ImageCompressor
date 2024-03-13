{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Parsing
-}

module Parsing (Options(..),
                defaultOpt,
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

defaultOpt :: Options
defaultOpt = Options {
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

isNotOpt :: Maybe Options -> Maybe Options
isNotOpt (Just opt)
    |   isNothing (colorNb opt) || isNothing (convergence opt) ||
        isNothing (filePath opt) = Nothing
    |   otherwise = Just opt
isNotOpt _ = Nothing

checkEmpty :: ([Options -> Options], [String], [String]) -> Maybe Options
checkEmpty (opts, [], []) = isNotOpt (Just (foldl (flip id) defaultOpt opts))
checkEmpty _ = Nothing

parseArgs :: [String] -> Maybe Options
parseArgs args = checkEmpty (getOpt Permute options args)

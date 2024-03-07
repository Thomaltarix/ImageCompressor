{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Parsing (parseArgs)
import FileParsing (fileParsing)
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> hPutStrLn stderr "Invalid input" >> exitWith(ExitFailure 84)
        Just opt -> fileParsing opt
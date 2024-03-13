{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Parsing (parseArgs, Options(..))
import FileParsing (getFile, fillConf)
import ImageCompressor (imageCompressor)
import System.Environment
import System.Exit
import System.IO

exit :: String -> IO ()
exit str = hPutStrLn stderr str >> exitWith (ExitFailure 84)

getOptionsFilePath :: Maybe Options -> String
getOptionsFilePath (Just opt) = case filePath opt of
    Just path -> path
    Nothing -> ""
getOptionsFilePath _ = ""

main :: IO ()
main = do
    args <- getArgs
    let opt = parseArgs args
    content <- getFile (Just (getOptionsFilePath opt))
    case content of
        Nothing -> exit "Error: file not found"
        Just content' -> imageCompressor (fillConf content') opt

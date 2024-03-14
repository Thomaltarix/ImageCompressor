{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- ImageCompressor
-}

module ImageCompressor (imageCompressor) where

import FileParsing (Conf(..))
import Parsing (Options(..))

imageCompressor :: Maybe Conf -> Maybe Options -> IO ()
imageCompressor (Just _) (Just _) = putStrLn "Success"
imageCompressor Nothing _ = putStrLn "Wrong file content"
imageCompressor _ _ = putStrLn "Wrong Options"

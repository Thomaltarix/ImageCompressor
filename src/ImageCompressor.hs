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
imageCompressor (Just _) (Just _) = putStrLn "imageCompressor no error"
imageCompressor _ _ = putStrLn "imageCompressor error"

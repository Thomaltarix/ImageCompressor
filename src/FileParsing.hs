{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- FileParsing
-}

module FileParsing (fileParsing) where
data Vector = Vector [Double] deriving (Show)

data Conf = Conf {
    coordinates :: [Vector],
    colors :: [Vector]
} deriving (Show)

import Parsing (Options(..))
getFile :: Maybe String -> IO (Maybe String)
getFile (Just path) = do
    file <- try $ openFile path ReadMode :: IO (Either SomeException Handle)
    case file of
        Left _ -> return Nothing
        Right file -> do
            content <- hGetContents file
            return $ Just content
getFile _ = return Nothing

fileParsing :: Options -> IO ()
fileParsing opt = return ()

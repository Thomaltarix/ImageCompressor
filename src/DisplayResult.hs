{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Display
-}

module DisplayResult (displayResult) where

import Vector (Vector(..))
import CentroidBind (CentroidBind(..))
import GHC.Float

putVector :: Vector -> IO ()
putVector (Vector []) = return ()
putVector (Vector l) =
  putStr "(" >>
  putVector' l >>
  putStr ")"
  where
    putVector' [] = return ()
    putVector' [x] = putStr (show (double2Int x))
    putVector' (x:xs) =
      putStr (show (double2Int x) ++ ",") >> putVector' xs

displayAllVectors :: [(Vector, Vector)] -> IO ()
displayAllVectors [] = return ()
displayAllVectors ((coord, vec):xs) =
    putVector coord >>
    putStr " " >>
    putVector vec >>
    putStrLn "" >>
    displayAllVectors xs

displayResult :: [CentroidBind] -> IO ()
displayResult [] = return ()
displayResult ((CentroidBind centroid lPair):xs) =
    putStrLn "--" >>
    putVector centroid >>
    putStrLn "\n-" >>
    displayAllVectors lPair >>
    displayResult xs

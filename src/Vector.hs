{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Parsing
-}

module Vector (Vector(..), getDistance) where

data Vector = Vector [Double] deriving (Show)

instance Eq Vector where
    (Vector x) == (Vector y) = getDistance (Vector x) (Vector y) < 0.001
    (Vector x) /= (Vector y) = getDistance (Vector x) (Vector y) > 0.001

getDistance :: Vector -> Vector -> Double
getDistance (Vector l1) (Vector l2) =
  sqrt (sum (zipWith (\a b -> (a - b) ** 2) l1 l2))

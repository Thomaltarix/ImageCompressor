{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Vector
-}

module Vector (Vector(..), getDistance, getClosestVector, roundVector) where

import GHC.Float

data Vector = Vector [Double] deriving (Show)

instance Eq Vector where
    (Vector x) == (Vector y) = getDistance (Vector x) (Vector y) < 0.001
    (Vector x) /= (Vector y) = getDistance (Vector x) (Vector y) > 0.001

getDistance :: Vector -> Vector -> Double
getDistance (Vector l1) (Vector l2) =
  sqrt (sum (zipWith (\a b -> (a - b) ** 2) l1 l2))

getClosestVector :: Vector -> [Vector] -> Vector
getClosestVector _ [] = error "Empty list"
getClosestVector _ [a] = a
getClosestVector v (a:xs)
  | getDistance v a < getDistance v (getClosestVector v xs) = a
  | otherwise = getClosestVector v xs

roundVector :: Vector -> Vector
roundVector (Vector l) =
  Vector (map (\x -> int2Double (double2Int (x + 0.5))) l)

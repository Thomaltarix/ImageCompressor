{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- CentroidBind
-}

module CentroidBind (CentroidBind(..),
                     bindVectorToCentroid,
                     emptyCentroid) where

import Vector (Vector(..), getClosestVector)

data CentroidBind = CentroidBind Vector [(Vector, Vector)] deriving (Show)

getCentroidFromBindList :: [CentroidBind] -> [Vector]
getCentroidFromBindList [] = []
getCentroidFromBindList ((CentroidBind centroid _):xs) =
  centroid : getCentroidFromBindList xs

bindVectorToCentroid :: (Vector, Vector) -> [CentroidBind] -> [CentroidBind]
bindVectorToCentroid _ [] = []
bindVectorToCentroid (coord, vec) ((CentroidBind centroid lPair):xs)
  | centroid == getClosestVector vec
    (getCentroidFromBindList ((CentroidBind centroid lPair):xs)) =
      (CentroidBind centroid ((coord, vec):lPair)) : xs
  | otherwise =
    (CentroidBind centroid lPair) : bindVectorToCentroid (coord, vec) xs

emptyCentroid :: CentroidBind -> CentroidBind
emptyCentroid (CentroidBind centroid _) = CentroidBind centroid []

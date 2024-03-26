{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- ImageCompressor
-}

module ImageCompressor (imageCompressor, getDistance) where

import FileParsing (Conf(..))
import Parsing (Options(..))
import GHC.Float
import Vector (Vector(..), getDistance, roundVector)
import DisplayResult (displayResult)
import CentroidBind (CentroidBind(..),
                     bindVectorToCentroid, emptyCentroid)

getNbColors :: Options -> Int
getNbColors opt = case colorNb opt of
    Just nb -> nb
    Nothing -> 0

getConvergence :: Options -> Double
getConvergence opt = case convergence opt of
    Just conv -> conv
    Nothing -> 0.0

doStep :: [CentroidBind] -> [(Vector, Vector)] -> [CentroidBind]
doStep centroids vectors =
  foldl (\c v -> bindVectorToCentroid v c) centroids vectors

getWeightedCenter :: Vector -> Int -> Vector -> Int -> Vector
getWeightedCenter (Vector vec1) n (Vector vec2) m =
  Vector (zipWith (\a b -> (a * int2Double n + b * int2Double m) /
  int2Double (n + m)) vec1 vec2)

getCenter :: [Vector] -> Vector
getCenter [] = Vector [128, 128, 128]
getCenter [v] = v
getCenter (v:xs) = getWeightedCenter v 1 (getCenter xs) n where n = length xs

recenter :: [CentroidBind] -> [CentroidBind]
recenter [] = []
recenter ((CentroidBind _ lPair):xs) =
  (CentroidBind (getCenter (map snd lPair)) lPair) : recenter xs

compareCentroids :: [CentroidBind] -> [CentroidBind] -> Double -> Bool
compareCentroids [] [] _ = True
compareCentroids [] _ _ = False
compareCentroids _ [] _ = False
compareCentroids
  ((CentroidBind centroid1 _):xs) ((CentroidBind centroid2 _):ys) conv
    | getDistance centroid1 centroid2 < conv = compareCentroids xs ys conv
    | otherwise = False

doImageCompressor :: Conf -> Options -> [CentroidBind] -> IO ()
doImageCompressor (Conf lconf) opt centroids =
  let recentered = recenter (doStep centroids lconf) in
  if compareCentroids centroids recentered (getConvergence opt)
  then
    displayResult [CentroidBind (roundVector newCentroid)
      (map (\(coord, vec) -> (roundVector coord, roundVector vec)) newLPair) |
      (CentroidBind newCentroid newLPair) <- recentered]
  else
    doImageCompressor (Conf lconf) opt [emptyCentroid newCentroid |
      newCentroid <- recentered]

createCentroidsBasedOnConf :: Conf -> Int -> [CentroidBind]
createCentroidsBasedOnConf (Conf lconf) nbColors =
  [CentroidBind (vector) [] | vector <- take nbColors (map snd lconf)]

imageCompressor :: Maybe Conf -> Maybe Options -> IO ()
imageCompressor (Just c) (Just opt) =
  doImageCompressor c opt (createCentroidsBasedOnConf c (getNbColors opt))
imageCompressor Nothing _ = putStrLn "Wrong file content"
imageCompressor _ _ = putStrLn "Wrong Options"

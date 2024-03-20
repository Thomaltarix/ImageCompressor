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
import Vector (Vector(..), getDistance)

data CentroidBind = CentroidBind Vector [(Vector, Vector)] deriving (Show)

createRandomCentroids :: Int -> [CentroidBind]
createRandomCentroids 0 = []
createRandomCentroids n =
  CentroidBind (Vector [128, 128, 128]) [] : createRandomCentroids (n - 1)

getCentroidFromBindList :: [CentroidBind] -> [Vector]
getCentroidFromBindList [] = []
getCentroidFromBindList ((CentroidBind a _):xs) =
  a : getCentroidFromBindList xs

bindVectorToCentroid :: (Vector, Vector) -> [CentroidBind] -> [CentroidBind]
bindVectorToCentroid _ [] = []
bindVectorToCentroid (co, v) ((CentroidBind c l):xs) =
  if getClosestVector v (getCentroidFromBindList ((CentroidBind c l):xs)) == c
  then (CentroidBind c ((co, v):l)) : xs
  else (CentroidBind c l) : bindVectorToCentroid (co, v) xs

getClosestVector :: Vector -> [Vector] -> Vector
getClosestVector _ [] = error "Empty list"
getClosestVector _ [a] = a
getClosestVector v (a:xs)
  | getDistance v a < getDistance v (getClosestVector v xs) = a
  | otherwise = getClosestVector v xs

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
getWeightedCenter (Vector l1) n (Vector l2) m =
  Vector (zipWith (\a b -> (a * int2Double n + b * int2Double m) /
  int2Double (n + m)) l1 l2)

getCenter :: [Vector] -> Vector
getCenter [] = Vector [128, 128, 128]
getCenter [v] = v
getCenter (v:xs) = getWeightedCenter v 1 (getCenter xs) n where n = length xs

recenter :: [CentroidBind] -> [CentroidBind]
recenter [] = []
recenter ((CentroidBind _ l):xs) =
  (CentroidBind (getCenter (map snd l)) l) : recenter xs

compareCentroids :: [CentroidBind] -> [CentroidBind] -> Double -> Bool
compareCentroids [] [] _ = True
compareCentroids [] _ _ = False
compareCentroids _ [] _ = False
compareCentroids ((CentroidBind c1 _):xs) ((CentroidBind c2 _):ys) conv =
  if getDistance c1 c2 < conv then compareCentroids xs ys conv else False

roundVector :: Vector -> Vector
roundVector (Vector l) =
  Vector (map (\x -> int2Double (double2Int (x + 0.5))) l)

emptyCentroid :: CentroidBind -> CentroidBind
emptyCentroid (CentroidBind c _) = CentroidBind c []

doImageCompressor :: Conf -> Options -> [CentroidBind] -> IO ()
doImageCompressor (Conf lc) opt centroids =
  let recentered = recenter (doStep centroids lc) in
  if compareCentroids centroids recentered (getConvergence opt)
  then
    displayResult [CentroidBind (roundVector c)
      (map (\(a, b) -> (roundVector a, roundVector b)) l) |
      (CentroidBind c l) <- recentered]
  else
    doImageCompressor (Conf lc) opt [emptyCentroid c | c <- recentered]

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
displayAllVectors ((a, b):xs) =
    putVector a >>
    putStr " " >>
    putVector b >>
    putStrLn "" >>
    displayAllVectors xs

displayResult :: [CentroidBind] -> IO ()
displayResult [] = return ()
displayResult ((CentroidBind c l):xs) =
    putStrLn "--" >>
    putVector c >>
    putStrLn "\n-" >>
    displayAllVectors l >>
    displayResult xs

imageCompressor :: Maybe Conf -> Maybe Options -> IO ()
imageCompressor (Just c) (Just opt) =
  doImageCompressor c opt (createRandomCentroids (getNbColors (opt)))
imageCompressor Nothing _ = putStrLn "Wrong file content"
imageCompressor _ _ = putStrLn "Wrong Options"

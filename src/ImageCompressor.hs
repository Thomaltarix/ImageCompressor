{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- ImageCompressor
-}

module ImageCompressor (imageCompressor) where

import FileParsing (Conf(..), Vector(..))
import Parsing (Options(..))
import GHC.Float

data CentroidBind = CentroidBind Vector [Vector] deriving (Show)

instance Eq Vector where
    (Vector x) == (Vector y) = getDistance (Vector x) (Vector y) < 0.001
    (Vector x) /= (Vector y) = getDistance (Vector x) (Vector y) > 0.001

createRandomCentroids :: Int -> [CentroidBind]
createRandomCentroids 0 = []
createRandomCentroids n = CentroidBind (Vector [128, 128, 128]) [] : createRandomCentroids (n - 1)

getColorVectorFromConf :: Conf -> [Vector]
getColorVectorFromConf (Conf l) = map (\(_, b) -> b) l

getCoordVectorFromConf :: Conf -> [Vector]
getCoordVectorFromConf (Conf l) = map (\(a, _) -> a) l

getCentroidFromBindList :: [CentroidBind] -> [Vector]
getCentroidFromBindList [] = []
getCentroidFromBindList ((CentroidBind a _):xs) = a : getCentroidFromBindList xs

bindVectorToCentroid :: Vector -> [CentroidBind] -> [CentroidBind]
bindVectorToCentroid _ [] = []
bindVectorToCentroid v ((CentroidBind c l):xs) =
    if getClosestVector v (getCentroidFromBindList ((CentroidBind c l):xs)) == c
    then (CentroidBind c (v:l)) : xs
    else (CentroidBind c l) : bindVectorToCentroid v xs

getDistance :: Vector -> Vector -> Double
getDistance (Vector l1) (Vector l2) =
    sqrt (sum (zipWith (\a b -> (a - b) ** 2) l1 l2))

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

getContentfromConf :: Conf -> [(Vector, Vector)]
getContentfromConf (Conf l) = l

doStep :: [CentroidBind] -> [Vector] -> [CentroidBind]
doStep centroids vectors = foldl (\acc v -> bindVectorToCentroid v acc) centroids vectors

getWeightedCenter :: Vector -> Int -> Vector -> Int -> Vector
getWeightedCenter (Vector l1) n (Vector l2) m = Vector (zipWith (\a b -> (a * int2Double n + b * int2Double m) / int2Double (n + m)) l1 l2)

getCenter :: [Vector] -> Vector
getCenter [] = Vector [128, 128, 128]
getCenter [v] = v
getCenter (v:xs) = getWeightedCenter v 1 (getCenter xs) n where n = length xs

recenter :: [CentroidBind] -> [CentroidBind]
recenter [] = []
recenter ((CentroidBind _ l):xs) = (CentroidBind (getCenter l) []) : recenter xs

compareCentroids :: [CentroidBind] -> [CentroidBind] -> Double -> Bool
compareCentroids [] [] _ = True
compareCentroids [] _ _ = False
compareCentroids _ [] _ = False
compareCentroids ((CentroidBind c1 _):xs) ((CentroidBind c2 _):ys) conv =
    if getDistance c1 c2 < conv then compareCentroids xs ys conv else False

roundVector :: Vector -> Vector
roundVector (Vector l) = Vector (map (\x -> int2Double (double2Int (x + 0.5))) l)

doImageCompressor :: Conf -> Options -> [CentroidBind] -> IO ()
doImageCompressor conf opt centroids = do
    let vectors = getColorVectorFromConf conf
    let newCentroids = doStep centroids vectors
    putStrLn "New centroids: "
    print newCentroids
    let recentered = recenter newCentroids
    putStrLn "Recentered: "
    print recentered
    if compareCentroids centroids recentered (getConvergence opt)
    then putStrLn "Success" >>
        displayResult (getContentfromConf conf) (zipWith (\(CentroidBind c _) (CentroidBind _ l) -> CentroidBind (roundVector c) l) recentered newCentroids)
    else doImageCompressor conf opt recentered

putVector :: Vector -> IO ()
putVector (Vector []) = return ()
putVector (Vector l) = do
    putStr "("
    putVector' l
    putStr ")"
    where
        putVector' [] = return ()
        putVector' [x] = putStr (show x)
        putVector' (x:xs) = putStr (show x ++ ", ") >> putVector' xs

getIndexOfVector :: Vector -> [Vector] -> Int
getIndexOfVector _ [] = 0
getIndexOfVector v (x:xs) = if v == x then 0 else 1 + getIndexOfVector v xs

displayAllVectors :: [(Vector, Vector)] -> IO ()
displayAllVectors [] = return ()
displayAllVectors ((a, b):xs) = do
    putVector a
    putVector b
    putStrLn ""
    displayAllVectors xs

displayResult :: [(Vector, Vector)] -> [CentroidBind] -> IO ()
displayResult _ [] = return ()
displayResult l ((CentroidBind c lc):xs) = do
    putStrLn "Centroid: "
    putVector c
    putStrLn "\nClosest vectors: "
    displayResult l xs

imageCompressor :: Maybe Conf -> Maybe Options -> IO ()
imageCompressor (Just c) (Just opt) = putStrLn "Success" >>
    doImageCompressor c opt (createRandomCentroids (getNbColors (opt)))
imageCompressor Nothing _ = putStrLn "Wrong file content"
imageCompressor _ _ = putStrLn "Wrong Options"

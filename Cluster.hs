module Cluster(
  startClusterization
  ) where

  type Point = [Double]
  type PointCollection = [Point]

  type ClusterizationResultItem = [Double]
  type ClusterizationResult = [ClusterizationResultItem]

  type ClusterItem = (Point, Integer)
  type Cluster = [ClusterItem]

  distance :: Point -> Point -> Double
  distance x y = sqrt $ sum $ map (**2) $ zipWith (-) x y

  startClusterization :: Integer -> PointCollection -> ClusterizationResult
  startClusterization number = map toResult . clusterization . map rangeCluster . zip [0..]
    where
      rangeCluster (i, point) = (point, clusterNumber i)
      clusterNumber i = mod i number
      toResult (point, i) = (fromIntegral i):point

  clusterization :: Cluster -> Cluster
  clusterization x = x

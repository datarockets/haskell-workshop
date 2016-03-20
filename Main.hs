module Main where

import Control.Exception
import Prelude
import Numeric
import Data.List
import Data.List.Split
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Data.Ord

import Data.CSV.Conduit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

-- =====  k-means =====

type Point = [Double]
type PointVector = [Point]

type Centers = [(Point, Integer)]

type ClusterItem = (Point, Integer)
type Cluster = [ClusterItem]

distance :: Point -> Point -> Double
distance x y = sqrt $ sum $ map (**2) $ zipWith (-) x y

clustarizationStart :: Integer -> PointVector -> Cluster
clustarizationStart n = clustarization . map rangeCluster . zip [0..]
  where
    rangeCluster (i, xs) = (xs, clusterNumber i)
    clusterNumber i = mod i n

clustarization :: Cluster -> Cluster
clustarization cluster
  | difference == 0 = new_cluster
  | otherwise = clustarization new_cluster
  where
    new_cluster = clustarizationStep cluster
    difference = sum $ zipWith differenceClusters cluster new_cluster
    differenceClusters (_, x) (_, y) = if x == y then 0 else 1

clustarizationStep :: Cluster -> Cluster
clustarizationStep cluster = map (searchCluster centers) points
  where
    points = map (\x -> fst x) cluster
    centers = searchCenters cluster

searchCenters :: Cluster -> Centers
searchCenters = map searchPoint . createGroups
  where
    createGroups = map grouping . groupBy (\x y -> snd x == snd y) . sortBy (comparing snd)
    grouping xs = (map fst xs, snd $ head xs)
    searchPoint (points, n) = (calculatePoint points, n)
    calculatePoint = map calculateValue . transpose
    calculateValue values = (sum values) / (fromIntegral(length values))

searchCluster :: Centers -> Point -> ClusterItem
searchCluster centers point = (point, clusterNumber)
  where
    clusterNumber = snd $ minimum $ map findDinstance centers
    findDinstance (value, number) = (distance value point, number)

-- =====  parsing =====

handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle

convertFromCsv :: V.Vector (Row String) -> [[Double]]
convertFromCsv = processCsv . V.toList
    where processCsv = filter (not . null) . map processRow
          processRow = map (fromMaybe 0.0) . filter isJust . map maybeRead
          maybeRead = fmap fst . listToMaybe . (reads :: String -> [(Double, String)])

-- =====  output  =====

convertToCsv :: [[Double]] -> [B.ByteString]
convertToCsv= intersperse (BS.pack [13, 10]) . map (B.pack . intercalate "," . map ((\f -> f "") . showFFloat (Just 6)))

-- =====  main  =====

main :: IO ()
main = do
  let csvOpts = defCSVSettings {csvSep = ',', csvQuoteChar = Nothing}
  input <- handleAll (\e -> error $ "Cannot read input file: " ++ show e ) $ runResourceT $ readCSVFile csvOpts "example.txt"

  let result = map (\x -> (fromInteger(snd x)):(fst x)) $ clustarizationStart 2 $ convertFromCsv input

  runResourceT $ CL.sourceList (convertToCsv result) $$ CB.sinkIOHandle $ return stdout

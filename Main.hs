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

import Data.CSV.Conduit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL


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

  let result = convertFromCsv input

  runResourceT $ CL.sourceList (convertToCsv result) $$ CB.sinkIOHandle $ return stdout

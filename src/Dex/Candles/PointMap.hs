module Dex.Candles.PointMap (
  PointMap,
  addPoint,
  readPoints,
  extractPoints,
  removeOldPoints,
  removePointsThread,
  groupPoints,
) where

import Control.Concurrent.STM
import Control.Monad (forever, when)
import Data.Function (on)
import Data.Hashable (Hashable(hash))
import Data.List (sortBy, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)

import Dex.Candles.Time
import Dex.Candles.Types
import Dex.STM.Time (TTime)
import Dex.Types.AssetMap (AssetClass)

import Focus qualified as F
import ListT qualified as LT
import StmContainers.Map qualified as SM
import System.IO (hPutStrLn, stderr)


type PointMap = SM.Map PeriodIndex [CandlePoint]

-- | Add a point to the map, based on its period.
addPoint :: PointMap -> CandlePoint -> STM ()
addPoint pm cp = SM.focus (F.alter altr) prd pm
  where 
    prd = candPeriod cp
    altr :: Maybe [CandlePoint] -> Maybe [CandlePoint]
    altr Nothing    = Just [cp]
    altr (Just cps) = Just (cp:cps)

-- | Get all the points from a certain period.
-- This removes them from the map.
extractPoints :: PointMap -> PeriodIndex -> STM [CandlePoint]
extractPoints pm pdx = do
  cps <- fromMaybe [] <$> SM.lookup pdx pm
  SM.delete pdx pm
  return cps

-- | Read the points at an index. This
-- is more for troubleshooting than
-- anything.
readPoints :: PointMap -> PeriodIndex -> STM [CandlePoint]
readPoints pm pdx =
  concat <$> SM.lookup pdx pm

removePointsThread :: TTime -> PointMap -> Int -> Int -> Bool -> IO ()
removePointsThread _ _ _ _ False = return ()
removePointsThread ttm pmap len' bac' True = forever $ do
  prd <- getCurrentPeriodT' ttm
  let prd0 = prd - bac
      prd2 = prd + len
  bl <- atomically $ removeOldPoints pmap prd0
  when bl $ hPutStrLn stderr "Deleted some old points"
  waitUntilAfterPeriodT ttm prd2 (basePeriodTime / 10)
  where
    len = fromIntegral $ max len' 2
    bac = fromIntegral $ max bac' 2

-- | Remove any points from before a
-- specific `PeriodIndex`.
removeOldPoints :: PointMap -> PeriodIndex -> STM Bool
removeOldPoints pm pdx = do
  indxs <- LT.fold fldr [] (fst <$> SM.listT pm)
  mapM_ (`SM.delete` pm) indxs
  return (not $ null indxs)
  where
    fldr :: [PeriodIndex] -> PeriodIndex -> STM [PeriodIndex] -- -> [PeriodIndex] -> LT.ListT STM a -> STM [PeriodIndex]
    fldr pis pix
      | pix < pdx = return (pix:pis)
      | otherwise = return  pis

-- | Group `CandlePoint`s by AssetClass.
groupPoints :: [CandlePoint] -> [(AssetClass,[CandlePoint])]
groupPoints cps = mapMaybe mapFunc cpsGrouped
  where
    cpsSorted = sortBy (compare `on` (hash . candAsset)) cps
    cpsGrouped = groupBy ((==) `on` candAsset) cpsSorted
    mapFunc :: [CandlePoint] -> Maybe (AssetClass,[CandlePoint])
    mapFunc [] = Nothing
    mapFunc lst@(x:xs) = Just (candAsset x, lst)



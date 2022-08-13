module Dex.Candles.CandleMap (
  TokenConfig(..),
  CandleMap,
  insertCandlePoint,
  insertCandlePoints,
  assetHandleThreads,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (mapM_, forever, when, unless)
import Data.Text qualified as T
import Dex.Async
import Dex.Candles.Time
import Dex.Candles.Time.Timeframe
import Dex.Candles.Types
import Dex.IO
import Dex.STM.TPQueue
import Dex.Types.AssetMap (AssetClass)
import GHC.Generics (Generic)
import Focus             qualified as F
import ListT             qualified as LT
import StmContainers.Map qualified as SM
import Data.Kind (Type)
import Dex.STM.Time
import System.IO (hPutStrLn, stderr)


-- | A mapping from period (divided by length)
-- to candlepoints in that period. Unfortunately,
-- this can cause a lot of problems. This should
-- be bypassed for tokens whose timeframe is
-- only one period.
type CandleMap = SM.Map Timeframe [CandlePoint]

-- | Add a single `CandlePoint` into the `CandleMap`.
insertCandlePoint :: TokenConfig -> CandleMap -> CandlePoint -> STM ()
insertCandlePoint tcfg cmap cp =
  SM.focus (F.alter altr) prd cmap
  where
    len :: Integer
    len = fromIntegral $ tcLength tcfg
    prd :: Timeframe
    prd = candPeriod cp `tfDiv` len
    altr :: Maybe [CandlePoint] -> Maybe [CandlePoint]
    altr Nothing = Just [cp]
    altr (Just cps) = Just (cp:cps)

-- | Add multiple `CandlePoint`s into the `CandleMap`.
insertCandlePoints :: TokenConfig -> CandleMap -> [CandlePoint] -> STM ()
insertCandlePoints tcfg cmap = mapM_ (insertCandlePoint tcfg cmap)

-- | The thread to run when doing stuff...
-- This is the thread stored in the 
-- `Dex.Candles.ThreadMap`.
-- Note that this is not the thread itself;
-- it starts a thread that performs the 
-- calculations and returns its `Async` id.
assetHandleThreads :: 
  forall (tq1 :: Type -> Type) (tq2 :: Type -> Type).
  (TPQueue tq1, TPQueue tq2) =>
  ThreadManager
  -> ConSink
  -> tq1 [CandlePoint]
  -> tq2 (Either CandleError CandleStick)
  -> TTime
  -> TokenConfig
  -> IO (Async ())
-- Handle it differently if this token's
-- timeframe length is only 1 period.
assetHandleThreads dummy errSink tq tqout txTime tcfg@(TokenConfig { tcLength = 1 }) =
  startThread dummy (return ()) $ forever $ do
    cps <- atomically $ readQueue tq
    case cps of
      []     -> return ()
      (x:xs) -> do
        let pdi = candPeriod x
        atomically $ writeQueue tqout (createCandleStick 1 pdi cps)
assetHandleThreads dummy errSink tq tqout txTime tcfg =
  startThreadSetup dummy SM.newIO (atomically . SM.reset) $ \cmap -> do
    -- Might want to start this with withAsync instead, if
    -- you want a cleaner.
    -- Also might want to start a thread to periodically remove
    -- old data from the map.
    -- startThread dummy (return ()) (checkerThread cmap csink)
    withAsync (cullerThread errSink txTime tcfg cmap 4 2) $ \_ -> do
      withAsync (checkerThread cmap tcfg tqout txTime) $ \_ -> forever $ do
        atomically $ do
          cps <- readQueue tq
          insertCandlePoints tcfg cmap cps

-- checkerThread csink cmap tcfg tqout = newConHandleIO csink >>= \chnd -> forever $ do

-- | The other thread. It waits until a specific time,
-- and then collects the `CandlePoint`s into a single
-- `CandleStick`, which it then writes to a queue.
checkerThread :: 
  forall (tq :: Type -> Type).
  TPQueue tq =>
  CandleMap
  -> TokenConfig
  -> tq (Either CandleError CandleStick)
  -> TTime
  -> IO ()
-- checkerThread cmap tcfg tqout txTime = forever $ do
checkerThread cmap tcfg tqout txTime = do
  tfrm <- (`tfDiv` fromIntegral len) <$> getCurrentPeriod
  waitUntilAfterTimeframeT txTime tfrm (min 800 (2 * basePeriodTime / 3))
  cps <- atomically $ SM.focus F.lookupAndDelete tfrm cmap
  maybe
    (return ())
    -- (\cps' -> atomically $ writeQueue tqout (createCandleStick len (curP * fromIntegral len) cps'))
    (atomically . writeQueue tqout . createCandleStick len (getTimeframePeriod tfrm))
    cps
  checkerThread' cmap (incTimeframe tfrm) tcfg tqout txTime
  where
    len = tcLength tcfg

-- startThreadSetup dummy setup closer action

-- | The thread that `checkerThread` changes
-- to after the first run-through.
checkerThread' ::
  forall (tq :: Type -> Type).
  TPQueue tq =>
  CandleMap
  -> Timeframe
  -> TokenConfig
  -> tq (Either CandleError CandleStick)
  -> TTime
  -> IO ()
checkerThread' cmap tfrm tcfg tqout txTime = do
  -- curP <- (`div` fromIntegral len) <$> getCurrentPeriod
  waitUntilAfterTimeframeT txTime tfrm (min 800 (2 * basePeriodTime / 3))
  cps <- atomically $ SM.focus F.lookupAndDelete tfrm cmap
  maybe
    (return ())
    -- (\cps' -> atomically $ writeQueue tqout (createCandleStick len (curP * fromIntegral len) cps'))
    (atomically . writeQueue tqout . createCandleStick len (getTimeframePeriod tfrm))
    cps
  checkerThread' cmap (incTimeframe tfrm) tcfg tqout txTime
  where len = fromIntegral $ getTimeframeLength tfrm

-- | Cull old candles.
cullerThread :: ConSink -> TTime -> TokenConfig -> CandleMap -> Int -> Int -> IO ()
cullerThread errSink ttm tcfg cmap len' bac' = do
  prd <- getCurrentPeriodT' ttm
  let tf' = prd `tfDiv` fromIntegral (tcLength tcfg)
      tfr = tf' `decTimeframeBy` bac
      tf2 = tf' `incTimeframeBy` len
  atomically $ cullCandles' cmap tfr
  waitUntilAfterTimeframeT ttm tf2 (basePeriodTime / 10)
  cullerThread' errSink ttm (incTimeframe tf2) tcfg cmap len bac
  where 
    bac = max bac' 2
    len = max len' 2

cullerThread' :: ConSink -> TTime -> Timeframe -> TokenConfig -> CandleMap -> Int -> Int -> IO ()
cullerThread' errSink ttm tf' tcfg cmap len bac = do
  let tfr = tf' `decTimeframeBy` bac
      tf2 = tf' `incTimeframeBy` len
  indxs <- atomically $ cullCandles' cmap tfr
  unless (null indxs) $ atomically $ directFlush errSink $ 
    "Removing points for " ++ T.unpack (tcTicker tcfg) ++ ":\n" ++ show indxs
  waitUntilAfterTimeframeT ttm tf2 (basePeriodTime / 10)
  cullerThread' errSink ttm (incTimeframe tf2) tcfg cmap len bac
--   where 
--     bac = max bac' 2
--     len = max len' 2

-- | A thread that removes out-of-date candles.
cullCandles' :: CandleMap -> Timeframe -> STM [Timeframe]
cullCandles' cmap tfr = do
  indxs <- LT.fold fldr [] (fst <$> SM.listT cmap)
  mapM_ (`SM.delete` cmap) indxs
  return indxs
  where
    fldr :: [Timeframe] -> Timeframe -> STM [Timeframe]
    fldr pts ptf
      | ptf < tfr = return (ptf:pts)
      | otherwise = return  pts

{-
removeOldPoints :: PointMap -> PeriodIndex -> STM ()
removeOldPoints pm pdx = do
  indxs <- LT.fold fldr [] (fst <$> SM.listT pm)
  mapM_ (`SM.delete` pm) indxs
  where
    fldr :: [PeriodIndex] -> PeriodIndex -> STM [PeriodIndex] -- -> [PeriodIndex] -> LT.ListT STM a -> STM [PeriodIndex]
    fldr pis pix
      | pix < pdx = return (pix:pis)
      | otherwise = return  pis
-}
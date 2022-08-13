module Dex.Candles.Time (
  PeriodIndex(..),
  basePeriodTime,
  getPeriodIndex,
  timestampPeriod,
  getCurrentPeriod,
  getCurrentPeriodT,
  getCurrentPeriodT',
  getPeriodStart,
  getPeriodEnd,
  waitUntilPeriod,
  waitUntilPeriodEnd,
  waitUntilAfterPeriod,
  waitUntilAfterPeriodT,
  waitUntilAfterTimeframe,
  waitUntilAfterTimeframeT,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (when, unless)
import Data.Hashable (Hashable)
import Data.Time.Clock
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Dex.Candles.Time.Period
import Dex.Candles.Time.Timeframe
import Dex.STM.Time
import Dex.Types.Context (Timestamp)
import GHC.Generics (Generic)

-- | Calculate the `PeriodIndex` from a `POSIXTime`.
getPeriodIndex :: POSIXTime -> PeriodIndex
getPeriodIndex pt = Period $
  floor $ pt / basePeriodTime

-- | Calculate the `PeriodIndex` from a `Timestamp`.
timestampPeriod :: Timestamp -> PeriodIndex
timestampPeriod = getPeriodIndex . snd

-- | Get the current period from the system clock.
getCurrentPeriod :: IO PeriodIndex
getCurrentPeriod = getPeriodIndex <$> getPOSIXTime

-- | Get the current period from the `TTime`.
getCurrentPeriodT :: TTime -> IO PeriodIndex
getCurrentPeriodT tt = getPeriodIndex <$> atomically (getTTime tt)

-- | Get the current period from the `TTime`,
-- using `getTTimeIO`.
getCurrentPeriodT' :: TTime -> IO PeriodIndex
getCurrentPeriodT' tt = do
  tim <- getTTimeIO tt
  case tim of
    Nothing -> getPeriodIndex <$> atomically (getTTime tt)
    Just tm -> return (getPeriodIndex tm)

-- | Get the starting point of the period.
getPeriodStart :: PeriodIndex -> POSIXTime
getPeriodStart (Period prd) =
  basePeriodTime * fromIntegral prd

-- | Get the endtime of the period.
-- i.e. the start of the next period.
getPeriodEnd :: PeriodIndex -> POSIXTime
getPeriodEnd (Period prd) =
  basePeriodTime * (1 + fromIntegral prd)

-- | Sleep until the start of the specified period.
waitUntilPeriod :: PeriodIndex -> IO ()
waitUntilPeriod pdx = do
  cpd <- getCurrentPeriod
  when (cpd < pdx) $ do
    pst <- getPOSIXTime
    let pstrt = getPeriodStart pdx
        dtime = pstrt - pst
    threadDelay $ floor (dtime * 1_000_000)

-- | Sleep until the end of the specified period.
waitUntilPeriodEnd :: PeriodIndex -> IO ()
waitUntilPeriodEnd pdx = do
  cpd <- getCurrentPeriod
  when (cpd <= pdx) $ do
    pst <- getPOSIXTime
    let pend  = getPeriodEnd pdx
        dtime = pend - pst
    threadDelay $ floor (dtime * 1_000_000)

-- | Wait until @n@ seconds after 
-- the end of the period
waitUntilAfterPeriod :: PeriodIndex -> NominalDiffTime -> IO ()
waitUntilAfterPeriod pdx dt = do
  -- cpd <- getCurrentPeriod
  let pend = getPeriodEnd pdx
  pst <- getPOSIXTime
  when (pst <= (pend + dt)) $ do
    pst' <- getPOSIXTime -- probably unnecessary
    let dtime = pend + dt - pst'
    threadDelay $ floor (dtime * 1_000_000)

-- | Wait until @n@ seconds after 
-- the end of the period, using
-- `TTime`.
waitUntilAfterPeriodT :: TTime -> PeriodIndex -> NominalDiffTime -> IO ()
waitUntilAfterPeriodT txTime pdx dt = do
  -- cpd <- getCurrentPeriodT' txTime
  let pend = getPeriodEnd pdx
  pst <- atomically $ getTTime txTime
  when (pst <= pend + dt) $ do
    -- pst' <- atomically $ getTTime txTime
    let ftime = pend + dt
    waitUntil 3000 txTime ftime

-- | Wait until @n@ seconds after 
-- the end of the period, using
-- `TTime`.
waitUntilAfterPeriodT' :: TTime -> PeriodIndex -> NominalDiffTime -> IO ()
waitUntilAfterPeriodT' txTime pdx dt = do
  -- cpd <- getCurrentPeriodT' txTime
  let pend = getPeriodEnd pdx
  pst <- atomically $ getTTime txTime
  when (pst <= pend + dt) $ do
    -- pst' <- atomically $ getTTime txTime
    let dtime = pend + dt - pst
    threadDelay $ floor (dtime * 1_000_000)


-- | Wait until @n@ seconds after 
-- the end of the timeframe.
-- Note that in this case, 
-- `PeriodIndex` has already been
-- divided by len.
waitUntilAfterTimeframe :: Timeframe -> NominalDiffTime -> IO ()
waitUntilAfterTimeframe tf dt = do
  -- cpd <- (`div` len) <$> getCurrentPeriod
  pst <- getPOSIXTime
  let pend = getTimeframeEnd tf
  when (pst <= (pend + dt)) $ do
    pst' <- getPOSIXTime
    let dtime = pend + dt - pst
    threadDelay $ floor (dtime * 1_000_000)

-- | Wait until @n@ seconds after 
-- the end of the timeframe. This
-- is the newer version, waiting
-- until `TTime` is a specific
-- value.
waitUntilAfterTimeframeT :: TTime -> Timeframe -> NominalDiffTime -> IO ()
waitUntilAfterTimeframeT txTime tf dt = do
  -- cpd <- (`div` len) <$> getCurrentPeriodT' txTime
  pst <- atomically $ getTTime txTime
  let pend  = getTimeframeEnd tf
  when (pst <= pend + dt) $ do
    -- pst' <- atomically $ getTTime txTime
    let ftime = pend + dt
    waitUntil 3000 txTime ftime

-- | Wait until @n@ seconds after 
-- the end of the timeframe. This
-- is the older version, using
-- `threadDelay`.
waitUntilAfterTimeframeT' :: TTime -> Timeframe -> NominalDiffTime -> IO ()
waitUntilAfterTimeframeT' txTime tf dt = do
  -- cpd <- (`div` len) <$> getCurrentPeriodT' txTime
  pst <- atomically $ getTTime txTime
  let pend  = getTimeframeEnd tf
  when (pst <= pend + dt) $ do
    -- pst' <- atomically $ getTTime txTime
    let dtime = pend + dt - pst
    threadDelay $ floor (dtime * 1_000_000)

-- | Wait until the `TTime` counter is
-- past a certain point in time. Retry
-- every @n@ microseconds.
waitUntil :: Int -> TTime -> POSIXTime -> IO ()
waitUntil n ttm pend = do
  pst <- atomically $ getTTime ttm
  unless (pst >= pend) $ do
    threadDelay n
    waitUntil n ttm pend

-- | A simpler version of `waitUntil`
-- that just retries every time the
-- `TTime` value is updated.
waitUntil' :: TTime -> POSIXTime -> IO ()
waitUntil' ttm pend = atomically $ do
  pst <- getTTime ttm
  check (pst >= pend)
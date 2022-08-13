module Dex.STM.Time (
  TTime,
  newTTimeIO,
  updateTTime,
  updateTTimeIO,
  getTTime,
  getTTimeIO,
  getTTimeUTC,
  getTTimeUTCIO,
) where

import Control.Concurrent.STM
import Control.Monad (void, when, (>=>))
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- | For checking the time when
-- you need to synchronise to
-- a time that might be off.
type TTime = TVar (Maybe POSIXTime)

-- | Create a new `TTime` leaving
-- the value empty until it can
-- be filled.
newTTimeIO :: IO TTime
newTTimeIO = newTVarIO Nothing

-- | Create a new `TTime` using
-- the current time as the
-- initial value.
newTTimeSetIO :: IO TTime
newTTimeSetIO = do
  tim <- getPOSIXTime
  newTVarIO (Just tim)

-- | Update the time to a new value.
updateTTime :: TTime -> POSIXTime -> STM ()
updateTTime tt ptim = do
  x <- readTVar tt
  case x of
    Nothing -> writeTVar tt (Just ptim)
    Just ot -> when (ot < ptim) $ writeTVar tt (Just ptim)

updateTTimeIO :: TTime -> POSIXTime -> IO ()
updateTTimeIO tt ptim = do
  x <- readTVarIO tt
  case x of
    Nothing -> atomically $ writeTVar tt (Just ptim)
    Just ot -> when (ot < ptim) $ atomically $ do
    -- Once you get into the atomically block,
    -- double-check that the time listed is still
    -- less than the time you want to update to,
    -- since it might have changed if you had
    -- to retry.
      ot' <- getTTime tt
      when (ot' < ptim) $ writeTVar tt (Just ptim)

-- | Get the current `TTime`.
-- This will block if no time has
-- been written to it yet.
getTTime :: TTime -> STM POSIXTime
getTTime = readTVar >=> maybe retry return

-- | Like `getTTime`, but faster,
-- in the `IO` monad, and doesn't retry.
getTTimeIO :: TTime -> IO (Maybe POSIXTime)
getTTimeIO = readTVarIO

-- | Get the current time from a 
-- `TTime`, but as a `UTCTime`.
getTTimeUTC :: TTime -> STM UTCTime
getTTimeUTC = fmap posixSecondsToUTCTime . getTTime

-- | Like `getTTimeUTC`, but faster,
-- in the `IO` monad, and doesn't retry.
getTTimeUTCIO :: TTime -> IO (Maybe UTCTime)
getTTimeUTCIO = fmap (fmap posixSecondsToUTCTime) . readTVarIO

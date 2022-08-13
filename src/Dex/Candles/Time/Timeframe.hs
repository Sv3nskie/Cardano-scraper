module Dex.Candles.Time.Timeframe (
  Timeframe,
  getTimeframeIndex,
  getTimeframeLength,
  getTimeframePeriod,
  getTimeframeStart,
  getTimeframeEnd,
  toTimeframe,
  tfDiv,
  incTimeframe,
  incTimeframeBy,
  decTimeframeBy,
  inTimeframe,
) where

import Data.Kind (Type)
import Data.Hashable (Hashable)
import Dex.Candles.Time.Period
import GHC.Generics (Generic)
import Data.Time.Clock.POSIX (POSIXTime)

-- | A type to indicate which timeframe an operation is on.
data Timeframe =
  Timeframe { getTimeframeIndex :: Integer, getTimeframeLength :: Integer }
  deriving stock   (Show, Generic)
  -- deriving newtype (Integral, Num, Ord, Enum, Real, Hashable)

instance Hashable Timeframe

-- | Only checks that the starting point is the same.
-- Doesn't check Whether the length is the same. This
-- is to make it compatible with the instance for `Ord`.
instance Eq Timeframe where
  (Timeframe tf len) == (Timeframe tf' len')
    = tf * len == tf' * len'

instance Ord Timeframe where
  compare (Timeframe tf len) (Timeframe tf' len')
    = compare (tf * len) (tf' * len')
  (Timeframe tf len) <= (Timeframe tf' len')
    = tf * len <= tf' * len'

-- | Convert a `Period` into a `Timeframe`.
toTimeframe :: PeriodIndex -> Integer -> Timeframe
toTimeframe (Period prd) len
  | len < 1   = Timeframe prd 1
  | otherwise = Timeframe (prd `div` len) len

-- | Shorter version of `toTimeframe` for infix use.
tfDiv :: PeriodIndex -> Integer -> Timeframe
tfDiv = toTimeframe

-- | Get the first `Period` in a `Timeframe`.
getTimeframePeriod :: Timeframe -> PeriodIndex
getTimeframePeriod (Timeframe tf len) = Period (tf * len)

-- | Get the start time of a `Timeframe`.
getTimeframeStart :: Timeframe -> POSIXTime
getTimeframeStart (Timeframe prd len)
  = fromIntegral (prd * len) * basePeriodTime 

-- | Get the end time of a `Timeframe`.
getTimeframeEnd :: Timeframe -> POSIXTime
getTimeframeEnd (Timeframe prd len)
  = fromIntegral ((prd + 1) * len) * basePeriodTime 

-- | Increment the `Timeframe` index by 1.
incTimeframe :: Timeframe -> Timeframe
incTimeframe (Timeframe prd len) = Timeframe (prd + 1) len

-- | Increment the `Timeframe` index by @n@.
incTimeframeBy :: Timeframe -> Int -> Timeframe
incTimeframeBy (Timeframe prd len) n'
  = Timeframe (prd + n) len
  where n = fromIntegral $ max n' 0

-- | Decrement the `Timeframe` index by @n@.
decTimeframeBy :: Timeframe -> Int -> Timeframe
decTimeframeBy (Timeframe prd len) n'
  = Timeframe (prd - n) len
  where n = fromIntegral $ max n' 0

-- | Check whether a `POSIXTime` falls within
-- a certain timeframe.
inTimeframe :: POSIXTime -> Timeframe -> Bool
inTimeframe tim (Timeframe prd len)
  = strt <= tim && tim < end
  where
    strt = fromIntegral ( prd      * len) * basePeriodTime
    end  = fromIntegral ((prd + 1) * len) * basePeriodTime

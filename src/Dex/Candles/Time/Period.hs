module Dex.Candles.Time.Period (
  PeriodIndex(..),
  basePeriodTime,
) where

import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

-- | The amount of time, in seconds,
-- that a period should last. Each
-- token has a candle-time that is
-- an integer multiple of this time.
basePeriodTime :: NominalDiffTime
basePeriodTime = 1800 -- 30 minutes

-- | Indicating which period this belongs to.
newtype PeriodIndex =
  Period { getPeriod :: Integer }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Integral, Num, Ord, Enum, Real, Hashable)

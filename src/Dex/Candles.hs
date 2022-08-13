module Dex.Candles (
  basePeriodTime
) where

import Data.Time.Clock

-- Deprecated

-- | The amount of time, in seconds,
-- that a period should last. Each
-- token has a candle-time that is
-- an integer multiple of this time.
basePeriodTime :: NominalDiffTime 
basePeriodTime = 300 -- 5 minutes
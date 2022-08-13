module Dex.Database.Types.Candle (
  DBCandle(..),
  createDBCandle,
  convertDBCandle,
) where

import Data.Bson hiding (lookup)
import Data.Bson qualified as BSON
import Data.Int
import Data.Time.Clock.POSIX (POSIXTime)
import Dex.Candles.Types (CandleStick(..))
import Dex.Database.Parsers ((!??), (!?!))
import GHC.Generics (Generic)
import Dex.Candles.Time (getPeriodStart)
import Numeric (showFFloat)
import Dex.Database.Time (ptimeToVal, valToPtime, lookupTime)

-- | The type of Candlestick used in the Database.
data DBCandle = DBCandle
  { dbcTimestamp :: POSIXTime
  , dbcHigh   :: Double
  , dbcLow    :: Double
  , dbcOpen   :: Double
  , dbcClose  :: Double
  , dbcVolume :: Double -- Integer
  , dbcMarket :: Maybe Double
  } deriving (Show, Eq, Generic)

instance Val DBCandle where
  val dbc = Doc $
    [ "timestamp" := ptimeToVal (dbcTimestamp dbc) -- dbcTimestamp dbc
    , "high"   := Float (dbcHigh  dbc)
    , "low"    := Float (dbcLow   dbc)
    , "open"   := Float (dbcOpen  dbc)
    , "close"  := Float (dbcClose dbc)
    , "volume" =: dbcVolume dbc
    ] <> maybe [] (\mrc -> ["marketcap" := Float mrc]) (dbcMarket dbc)
  cast' (Doc doc) = DBCandle
    <$> lookupTime "timestamp" doc
    <*> doc !?! "high"
    <*> doc !?! "low"
    <*> doc !?! "open"
    <*> doc !?! "close"
    <*> doc !?! "volume"
    <*> Just (doc !?! "marketcap") -- double-encapsulation is correct.
  cast' _ = Nothing


convertDBCandle :: DBCandle -> Value
convertDBCandle dbc = Doc $
  [ "timestamp" := ptimeToVal (dbcTimestamp dbc) -- =: dbcTimestamp dbc
  , "high"   := Float (dbcHigh  dbc)
  , "low"    := Float (dbcLow   dbc)
  , "open"   := Float (dbcOpen  dbc)
  , "close"  := Float (dbcClose dbc)
  , "volume" =: showFFloat (Just 6) (dbcVolume dbc) ""
  ] <> maybe [] (\mrc -> ["marketcap" := Float mrc]) (dbcMarket dbc)

createDBCandle :: Maybe Double -> CandleStick -> DBCandle
createDBCandle totSup cs =
  DBCandle
    strp
    (csMax cs)
    (csMin cs)
    (csOpen  cs)
    (csClose cs)
    -- (fromIntegral $ csVolume cs)
    (csTotalAda cs)
    ((csClose cs * ) <$> totSup)
  where
    -- Endpoint of the candlestick.
    -- endp' = csPeriod cs + fromIntegral (csLength cs)
    -- endp  = getPeriodStart endp'
    strp  = getPeriodStart (csPeriod cs)


module Dex.Candles.Types (
  CandlePoint(..),
  CandleStick(..),
  ExType(..),
  exType,
  createCandleStick,
  CandleError(..),
  TokenConfig(..),
) where

import Data.ByteString qualified as BS
import Data.Function (on)
import Data.List -- (find)
import Data.Proxy 
import Data.Text       qualified as T
import Dex.Candles.Time
import Dex.Types.AssetMap (AssetClass(..))
import Dex.Types.Context (Timestamp)
import Dex.Types.Exchange (Exchange(..))
import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

-- | A single data point in a a bar of 
-- a candle plot.
data CandlePoint = CandlePoint
  { candAsset :: AssetClass
  , candTime :: Timestamp
  , candPeriod :: PeriodIndex
  , candTicker :: T.Text
  , candCost :: Double -- Ada per token.
  , candRate :: Double -- Tokens per Ada.
  , candType :: ExType
  , candHash :: BS.ByteString -- More to keep it unique
  , candDex :: T.Text
  -- TODO: Add absolute volume of Ada and Tokens
  -- (Difficult part is deciding whether to use
  --  decimaled value or raw value.)
  , candAda    :: Double -- decimalised
  , candTokens :: Double -- decimalised.
  , candLiquid :: Double -- decimalised
  , candLiqToken :: Maybe AssetClass
  } deriving stock (Show, Eq, Generic)

data ExType = Buy | Sell
  deriving stock (Show, Eq, Generic)

exType :: Exchange -> ExType
exType (ExBuy  {}) = Buy
exType (ExSell {}) = Sell

data CandleStick = CandleStick
  { csAsset  :: AssetClass
  , csPeriod :: PeriodIndex
  , csTicker :: T.Text
  , csLength :: Int -- number of periods
  , csVolume :: Int -- number of swaps
  , csBuys   :: Int
  , csSells  :: Int
  , csOpen   :: Double
  , csClose  :: Double
  , csMax :: Double 
  , csMin :: Double
  -- TODO : split into total tokens/ada bought/sold?
  , csTotalTokens :: Double
  , csTotalAda    :: Double
  , csLiquidity   :: Double -- Liquidity at close.
  , csLiqToken    :: Maybe AssetClass
  } deriving stock (Show, Eq, Generic)

-- | Collect a list of `CandlePoints` into
-- a signle `CandleStick`.
createCandleStick :: Int -> PeriodIndex -> [CandlePoint] -> Either CandleError CandleStick
createCandleStick _ _ [] = Left EmptyCandle
createCandleStick len pdx cps
  -- Check that all periods fall into the range
  | (Just bpd) <- find (\cp@(candPeriod -> per) -> not (pdx <= per && per < (pdx + fromIntegral len))) cps
  = Left $ BadPeriod pdx len (candPeriod bpd)
  | (Just bpd) <- find (\cp@(candAsset -> ast) -> ast /= mainAC) cps
  = Left $ WrongAsset mainAC (candAsset bpd)
  -- Very inefficient, but less awkward to use...
  -- But since I'm only traversing the list
  -- a fixed number of times, it's still O(n).
  | openPt  <- minimumBy (compare `on` candTime) cps
  , closePt <- maximumBy (compare `on` candTime) cps 
  , sms <- getSums 0 0 (candCost $ head cps) (candCost $ head cps) 0 0
  , (vol, bgt, sld, minCost, maxCost, totTok, totAda) <- sms cps
  , ltoks <- mapMaybe candLiqToken cps
  = Right $ CandleStick
    { csAsset = mainAC
    , csPeriod = pdx
    , csTicker = ticker
    , csLength = len
    , csVolume = vol
    , csBuys = bgt
    , csSells = sld
    , csOpen  = candCost openPt
    , csClose = candCost closePt
    , csMax = maxCost
    , csMin = minCost
    , csTotalTokens = totTok
    , csTotalAda = totAda
    , csLiquidity = candLiquid closePt
    , csLiqToken = head ltoks <$ guard (allEq ltoks)
    }
  where 
    mainAC = candAsset  $ head cps
    ticker = candTicker $ head cps
    allEq [] = False -- i.e. the check that it's not empty.
    allEq (x:xs) = all (== x) xs


-- | To avoid too many traversals.
getSums :: Int -> Int -> Double -> Double -> Double -> Double -> [CandlePoint] ->
    (Int,  Int ,  Int ,  Double ,  Double ,  Double ,  Double )
getSums bought sold minRate maxRate totTok totAda []
  = (bought + sold, bought, sold, minRate, maxRate, totTok, totAda)
getSums     bought  sold  minRate  maxRate  totTok  totAda  (cp:cps)
  = getSums bought' sold' minRate' maxRate' totTok' totAda'     cps
  where
    (bought',sold') = if candType cp == Buy then (bought + 1, sold) else (bought, sold + 1)
    minRate' = min minRate (candCost cp)
    maxRate' = max maxRate (candCost cp)
    totTok' = totTok + candTokens cp
    totAda' = totAda + candAda cp

-- | An error when trying to create a candle.
data CandleError
  = EmptyCandle -- Not really an error, just an empty candlestick.
  | BadPeriod PeriodIndex Int PeriodIndex -- A CandlePoint falls outside the period
  | WrongAsset AssetClass AssetClass -- More than one asset class in set.
  deriving (Show, Eq)


-- | Basic Config for how a Token-collector
-- thread should run.
data TokenConfig = TokenConfig
  { tcLength :: Int -- ^ Number of periods in one timeframe.
  , tcToken  :: AssetClass
  , tcTicker :: T.Text
  } deriving stock (Show, Eq, Generic)


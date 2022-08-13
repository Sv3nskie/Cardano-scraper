module Dex.Candles.Managers (
  handleCandles,
  outputCandles,
) where

import Control.Concurrent.STM
import Control.Monad (forever, when)
import Data.Kind (Type)
import Data.Text qualified as T
import Dex.Candles.PointMap
import Dex.Candles.Time
import Dex.Candles.Types
import Dex.IO
import Dex.STM.Time (TTime)
import Dex.STM.TPQueue
import Numeric (showFFloat)
import StmContainers.Map qualified as SM
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dex.Types (AssetClass)
import Dex.Types.KnownToken (KnownToken, ktDecimal)
import Dex.Database.Types.Setup (DBSetup, accessMaybe, accessCatch)
import Dex.Database.Actions (insertChart)
import Data.Maybe (isJust, fromMaybe)
import Dex.Database.SupplyCache (SupplyCache)


-- | Watch the map and flush it after the period ends.
handleCandles :: forall (tq :: Type -> Type). (TPQueue tq) =>
  tq [CandlePoint] -> TTime -> PointMap -> Bool -> IO ()
handleCandles _ _ _ False = return ()
handleCandles tq txTime pm True = do
  prd <- getCurrentPeriodT' txTime
  handleCandles' tq txTime prd pm
  -- Wait until 1/3 way into the next period.
  

handleCandles' :: forall (tq :: Type -> Type). (TPQueue tq) =>
  tq [CandlePoint] -> TTime -> PeriodIndex -> PointMap -> IO ()
handleCandles' tq txTime prd pm = do
  -- Wait until 1/2 way into the next period.
  waitUntilAfterPeriodT txTime prd (min 600 (basePeriodTime / 2))
  pts <- atomically $ extractPoints pm prd
  atomically $ writeQueue tq pts
  handleCandles' tq txTime (prd + 1) pm
  -- Note : the `runThreadMap` functions handles
  -- the grouping of points by `AssetClass`.

-- | Write the stuff to the terminal, and, in the
-- future, the database.
outputCandles :: forall (tq :: Type -> Type). (TPQueue tq) =>
  SupplyCache -> ConSink -> Maybe DBSetup -> SM.Map AssetClass KnownToken -> tq (Either CandleError CandleStick) -> Bool -> IO ()
outputCandles _ _ _ _ _ False = return ()
outputCandles supplyCache csink mSetup ktMap tq True = newConHandleIO csink >>= \chnd -> forever $ do
  rslt <- atomically $ readQueue tq
  case rslt of
    Left EmptyCandle -> return ()
    Left (BadPeriod spd len lpd) -> do
      atomically $ do
        cPutStrLnT chnd "Error with Candlestick:"
        cPutStrLn  chnd $ show lpd ++ " is not the Period Range " ++ show spd ++ " + " ++ show len
        cPutStrLnT chnd "----------"
      atomically $ cFlush chnd
    Left (WrongAsset ac1 ac2) -> do
      atomically $ do
        cPutStrLnT chnd "Error with Candlestick: Mismatched assets"
        cPrint chnd ac1
        cPrint chnd ac2
        cPutStrLnT chnd "----------"
      atomically $ cFlush chnd
    Right candle -> do
      mkt <- atomically $ SM.lookup (csAsset candle) ktMap
      let prd    = csPeriod candle
          pstrt  = getPeriodStart prd
          pend   = getPeriodStart (prd + fromIntegral (csLength candle))
          pstrt' = posixSecondsToUTCTime pstrt
          pend'  = posixSecondsToUTCTime pend
          decs   = fromIntegral . ktDecimal <$> mkt
          decs'  = fromMaybe 0 decs
          tlen   = T.length $ csTicker candle
          adaLen = max (tlen - 2) 1
          adaTik = "Total ADA" <> T.replicate adaLen " " <> ": "
          tikLen = max (4 - tlen) 1
          tokTik = "Total " <> csTicker candle <> T.replicate tikLen " " <> ": "
      atomically $ do
        cPutStrLnT chnd "Candlestick:"
        cPutStrT   chnd "Asset : "
        cPrint     chnd $ csAsset candle
        cPutStrT   chnd "Name : "
        cPutStrLnT chnd $ csTicker candle
        cPutStrT   chnd "Start Time : "
        cPrint     chnd pstrt'
        cPutStrT   chnd "End Time   : "
        cPrint     chnd pend'
        -- Kinda long...
        -- Note : The prices per token aren't limited
        -- to 6 decimal places since they're a rate,
        -- not an exact price.
        cPutStrT   chnd "Open  : "
        cPutStrLn  chnd $ showFFloat Nothing (csOpen  candle) " ADA"
        cPutStrT   chnd "Close : "
        cPutStrLn  chnd $ showFFloat Nothing (csClose candle) " ADA"
        cPutStrT   chnd "Max   : "
        cPutStrLn  chnd $ showFFloat Nothing (csMax   candle) " ADA"
        cPutStrT   chnd "Min   : "
        cPutStrLn  chnd $ showFFloat Nothing (csMin   candle) " ADA"
        cPutStrLn  chnd $ "Buys: " ++ show (csBuys candle) ++ ", Sells: " ++ show (csSells candle)
        cPutStrT   chnd adaTik -- TODO: adjust to same length as token.
        cPutStrLn  chnd $ showFFloat (Just 6) (csTotalAda candle) ""
        cPutStrT   chnd tokTik
        -- 'decs' is already in a Maybe.
        cPutStrLn  chnd $ showFFloat decs (csTotalTokens candle) ""
        cPutStrT   chnd "Liquidity Token : "
        cPrint     chnd $ csLiqToken candle
      
      -- Add to the database.
      dbRslt <- accessCatch mSetup (insertChart supplyCache "SUNDAE" candle decs')
      -- If the outer Maybe is Just, then
      -- the action was (attempted to be) run.
      -- Otherwise, the program didn't try to
      -- run the action, since it didn't have
      -- the username/password for the DB.
      -- when (isJust dbRslt) $ atomically $ do 
      atomically $ do 
        cPutStrLnT chnd "Result from DB: "
        cPrint     chnd dbRslt
      atomically $ cPutStrLnT chnd "----------"
      atomically $ cFlush chnd

{-
getPeriodStart :: PeriodIndex -> POSIXTime

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
  } deriving stock (Show, Eq, Generic)

-}

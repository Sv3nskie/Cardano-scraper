{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent (runInUnboundThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (catch, bracket)
import Control.Monad (forever, void, when, forM_, unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Numeric
import Servant
import System.IO
import Dex.Arguments
import Dex.Async
import Dex.Candles.Managers
import Dex.Candles.Types (ExType(..), exType, CandlePoint (CandlePoint), candTicker)
import Dex.Hex
import Dex.IO
import Dex.Identification
import Dex.Managers.Sundae
import Dex.Metadata (queryMetadata, queryMetadata', queryMetadataNew, queryMetadataNewRetry)
import Dex.STIO
import Dex.STM
import Dex.STM.Time
import Dex.STM.TPQueue
import Dex.Types
import Dex.Types.Component
import Dex.Types.Exchange (Exchange (ExBuy, ExSell), excAsset)
import Dex.Types.ExtraTransaction (ExtraTransaction(..))
import Dex.Types.FullTransaction(FullTransaction(..), PartialTransaction)
import Dex.Types.KnownToken
import ListT qualified as LT
import StmContainers.Map qualified as SM
import Dex.Managers.Sundae.Calculator (calculatePrice, getLiquidityToken, calculatePrice')
import Dex.Types.AssetMap (AssetClass (AssetClass))
import Network.HTTP.Client (Manager, ManagerSettings, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Printf
import Dex.Candles.Time (getPeriodIndex)
import Dex.Candles.PointMap (PointMap, addPoint, readPoints, removePointsThread)
import Dex.Candles.ThreadMap (runThreadMap)
import Dex.Database.Actions
import Dex.Database.Initialise (runDB)
import Dex.Database.Types.Setup (DBSetup, accessMaybe, accessCatch)
import Database.MongoDB.Connection (host)
import Dex.Database.Types.Exchange
import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Dex.Database.SupplyCache (SupplyCache)
import Control.Concurrent.QSem

type API = "upload" :> ReqBody '[JSON] TxEntry :> Post '[JSON] TxEntry

startApp :: InputArgs -> IO ()
startApp args = do
  tbq <- newTBQueueIO 500 -- temp
  tmm <- SM.newIO
  sunMap <- SM.newIO
  sundaeQ <- newTQueueIO
  manager <- newManager tlsManagerSettings

  -- cndInQ  <- newTBQueueIO 300 -- temporarily bounded
  ktMap   <- SM.newIO
  thrdMap <- SM.newIO
  cndInMp <- SM.newIO
  cndGrpQ <- newTQueueIO
  cstOutQ <- newTQueueIO

  supplyCache <- SM.newIO

  semLock <- newQSem 1

  -- Time var for stuff.
  txTime <- newTTimeIO

  -- when (isNothing $ iaUserAccess args) $ hPutStrLn stderr "Couldn't parse username/password."
  putStrLn $ "User name: " ++ show (uaName <$> iaUserAccess args)
  putStrLn "----------"

-- runThreadMap dummy ktMap thrMap cptQ cstQ

  -- min 60 means the max interval is an hour.
  let timeInterval = (60_000_000 *) . min 60 . max 0 <$> iaTimeCheck args
      candleChoice = iaRunCandles args

  -- To allow update of addresses.
  sunAddr1 <- newTVarIO sundaeAddr1
  sunAddr2 <- newTVarIO sundaeAddr2
  -- Lines with "-- C" are only run when
  -- candles are turned on.
  withConSink $ \conSink ->
    withConSinkH stderr $ \errSink ->
      runDB errSink (host "67.207.86.215") "ADA" (iaUserAccess args) semLock $ \mSetup -> do -- Main DB
      -- runDB errSink (host "164.92.155.46") "ADA" (iaUserAccess args) semLock $ \mSetup -> do -- Test DB
        withAsync (when candleChoice $ removePointsThread txTime cndInMp 3 3 candleChoice) $ \_ -> do -- C
          withAsync (runThreads $ \dummy -> queueReader dummy sunMap sunAddr1 sunAddr2 tbq sundaeQ) $ \_ -> do
            withAsync (handleCandles cndGrpQ txTime cndInMp candleChoice) $ \_ -> do -- C
              withAsync (when candleChoice (runThreads $ \dummy -> runThreadMap @TQueue dummy errSink ktMap thrdMap cndGrpQ cstOutQ txTime candleChoice)) $ \_ -> do -- C
                withAsync (when candleChoice $ outputCandles supplyCache conSink mSetup ktMap cstOutQ candleChoice) $ \_ -> do -- C
                  withAsync (timeChecker errSink txTime timeInterval) $ \_ -> do
                    withAsync (runThreads $ \dummy -> swapHandler dummy supplyCache mSetup manager conSink sunAddr1 sunAddr2 sundaeQ ktMap (iaKnownTokens args) cndInMp candleChoice) $ \_ -> do
                      -- withAsync (runThreads $ \dummy -> run 8080 (app dummy tmm tbq txTime)) (\_ -> dummyThread)
                      -- runThreads $ \dummy -> run 8080 (app dummy tmm tbq txTime)
                      runInUnboundThread (runThreads $ \dummy -> run 8080 (app dummy tmm tbq txTime))

-- In the future, we should probably just make a
-- record type that just contains all the various
-- TVars, TQueues, etc... we need for the server
-- so we don't have to change the signature each
-- time we add a new variable.

-- | Periodically output the current time,
-- if specified to do so in arguments.
timeChecker :: ConSink -> TTime -> Maybe Int -> IO ()
timeChecker _ _ Nothing = return ()
timeChecker _ _ (Just x) | x <= 0 = return ()
timeChecker snk ttime (Just x) = forever $ do
  ttim <- getTTimeUTCIO ttime
  stim <- getCurrentTime
  let ttm'  = maybe "<Not yet available>" show ttim
      outl1 = "Current System Time      : " ++ show stim ++ "\n"
      outl2 = "Current Transaction Time : " ++      ttm' ++ "\n"
      outl3 = "----------" -- directFlush adds \n
  atomically $ directFlush snk (outl1 <> outl2 <> outl3)
  threadDelay x

-- | Takes `FullTransaction`s from their queue
-- and pairs them up with the other half of the
-- transaction.
queueReader :: (TPQueue tq, TPQueue tq2) =>
  ThreadManager ->
  SM.Map TxInput ExtraTransaction ->
  TVar BS.ByteString ->
  TVar BS.ByteString ->
  tq FullTransaction ->
  tq2 SwapPair ->
  IO ()
queueReader dummy sunMap tun1 tun2 tbq swapQ = forever $ do
  ftx <- atomically $ readQueue tbq
  sunAddr1 <- readTVarIO tun1
  sunAddr2 <- readTVarIO tun2
  if
    | (hasOutputTo sunAddr1 ftx) -> do
        -- putStrLn "Got Part1:" >> putJSONLn ftx
        -- atomically $ addPart1 sunMap ftx
        -- Remove the item if no part 2 found
        -- within 15 minutes.
        -- Note that the time in addPart1Timeout is
        -- in milliseconds, not microseconds
        addPart1Timeout sunAddr1 dummy 900_000 sunMap ftx
    | (hasOutputTo sunAddr2 ftx) -> do
        -- putStrLn "Got Part2:" >> putJSONLn ftx

        -- Wait for 20 minutes.
        ctime <- getCurrentTime
        rslt' <- startThreadTimed 1_200_000 dummy (return ()) (atomically $ addPart2 sunMap ftx ctime)
        -- putStrLn "Matched a part2..."
        void $ startThreadE dummy (return ()) $ do
          rslt <- wait rslt'
          traverse (atomically . writeQueue swapQ) rslt
    | otherwise -> return ()

sundaeAddr1 :: BS.ByteString
sundaeAddr1 = "addr1wxaptpmxcxawvr3pzlhgnpmzz3ql43n2tc8mn3av5kx0yzs09tqh8"

sundaeAddr2 :: BS.ByteString
sundaeAddr2 = "addr1w9qzpelu9hn45pefc0xr4ac4kdxeswq7pndul2vuj59u8tqaxdznu"

-- c88bbd1848db5ea665b1fffbefba86e8dcd723b5085348e8a8d2260f44414e41
exampleAC :: AssetClass
exampleAC = AssetClass "44414e41" "c88bbd1848db5ea665b1fffbefba86e8dcd723b5085348e8a8d2260f"

{-
https://tokens.cardano.org/metadata/query
{"subjects" : ["c88bbd1848db5ea665b1fffbefba86e8dcd723b5085348e8a8d2260f44414e41"]
,"properties" : ["name", "ticker", "decimals"]
}
-}

-- | Handle the output from the `SwapPair` queue.
swapHandler :: forall (tq :: Type -> Type). (TPQueue tq) =>
  ThreadManager -> SupplyCache -> Maybe DBSetup -> Manager -> ConSink -> TVar BS.ByteString -> TVar BS.ByteString -> tq SwapPair -> SM.Map AssetClass KnownToken -> Maybe FilePath -> PointMap -> Bool -> IO ()
swapHandler dummy supplyCache mSetup manager csink tvAddr1 tvAddr2 swapQ ktMap mfp pntMap candleChoice =
  -- Try querying metadata with a basic token.
  -- This will cause an error right away if
  -- it fails, making troubleshooting the
  -- metadata querier easier.
  bracket (do
    -- kt <- queryMetadataNewRetry manager exampleAC
    -- print kt
    -- putStrLn "----------"
    -- Lookup the filepath
    case mfp of
      Nothing -> return ()
      Just fp -> do
        -- decodeFileStrict' returns (Maybe [a]);
        -- concat collapses that into [a].
        mkts <- concat <$> decodeFileStrict' @[MetaEntry] fp
        atomically $ forM_ mkts $ \ent -> do
          SM.insert (meData ent) (meToken ent) ktMap
    print mfp
    putStrLn "----------"
    return ktMap -- SM.newIO
    )
    ( \kmap -> do
      hPutStrLn stderr "Exiting swapHandler..."
      case mfp of
        Nothing -> return ()
        Just fp -> do
          lst <- atomically $ LT.toList $ uncurry MetaEntry <$> SM.listT kmap
          encodeFile @[MetaEntry] fp lst
          atomically $ SM.reset kmap
    ) $ \tokMap ->
    forever $ do
    pr <- atomically $ readQueue swapQ
    -- hPutStrLn stderr "Found something in the swapHandler."
    -- atomically $ directFlushT csink "Found something in the swapHandler."
    sunAddr1 <- readTVarIO tvAddr1
    sunAddr2 <- readTVarIO tvAddr2
    let val'  = calculatePrice' sunAddr1 sunAddr2 pr
        hash1 = txtHash . ftTransaction . etxTrans <$> swapPart1 pr
        hash2 = txtHash $ ftTransaction $              swapPart2 pr
        -- val   = fst <$> val'
        -- lqd   = snd <$> val' -- liquidity
        -- asst  = excAsset <$> val
        txmnt  = txtMintCount $ ftTransaction $ swapPart2 pr
        swtim  = swapTime pr
        tmstpX = ftTimestamp $ swapPart2 pr
    -- If the transaction mints anything, then
    -- the calculation is much more complicated.
    -- However, we can match up part1 transactions
    -- to the corresponding part of a batch
    -- transaction by matching the address of the 
    -- wallet.
    case val' of
      Nothing -> hPutStrLn stderr "Issue with calculator."
      Just ([],_,_,_,_,_,copies) -> do -- atomically $ directFlush csink $ 
        -- "Issue with tx2: " ++ show hash2
        chnd <- newConHandleIO csink
        atomically $ do
          cPutStrLnT chnd "Couldn't process Swap/Withdrawal/Deposit: "
          forM_ hash1 $ \hsh -> do
            cPutStrT chnd "Tx 1: "
            cPutStrLnBS chnd hsh -- uh...
          cPutStrT chnd "Tx 2: "
          cPutStrLnBS chnd hash2
          -- Output information regarding one wallet
          -- appearing in multiple transactions.
          unless (null copies) $ do
            cPutStrLnT chnd "Also, issues with same wallet appearing in multiple transactions:"
          forM_ copies $ \cpy -> do
            cPutStrT chnd "Tx: "
            cPutStrLnBS chnd (txtHash $ ftTransaction cpy)
          cPutStrLnT chnd "----------"
          cFlush chnd
      -- Spawns a new thread to perform the action.
      -- This is done since the metadata lookup can
      -- take a very long time, and blocking waiting
      -- for the response is a waste of time.
      Just (allSwaps, lqd, asst, adaLiq, poolAsset, failedTxs, multiTxs) -> do
        when (failedTxs > 0) $
          atomically $ directFlush csink $ "Swaps: " ++ show (length allSwaps) ++ "\nFailed: " ++ show failedTxs
        unless (null multiTxs) $ do
          let txt = withSTHandle_ $ \sthnd -> do
                sPutStrLnT sthnd "Aggregate Transactions where one wallet showed up multiple times: "
                forM_ multiTxs $ \tx -> do
                  sPutStrT  sthnd "Tx1: "
                  sPutStrLnBS sthnd (txtHash $ ftTransaction tx)
                sPutStrT sthnd "Tx2: "
                sPutStrLnBS sthnd hash2
                sPutStrT sthnd "----------"
          atomically $ directFlushT csink txt
        --   Async a' -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async c)
        forM_ allSwaps $ \(htx,val,walt) -> void $ startThreadSetupC 
          dummy 
          (newConHandleIO csink)
          -- Success:
          (\chnd -> atomically $ do { cPutStrLnT chnd "----------" ; cFlush chnd })
          -- Failure:
          (\chnd -> atomically $ do { cPutStrLnT chnd "Swap thread received exception." ; cPutStrLnT chnd "----------" ; cFlush chnd })
          -- Main Action:
          $ \chnd -> do
            -- atomically $ directFlushT csink "Working in the new thread."
            -- Lookup the Asset:
            asset <- tryLookup manager tokMap asst -- case asst of 
            let tmstp = ftTimestamp htx
                tmstpUTC = posixSecondsToUTCTime $ snd tmstp
                hsh1  = txtHash $ ftTransaction htx
            rats <- atomically $ do
              cPutStrT chnd "Tx 1: "
              cPutStrLnBS chnd hsh1
              cPutStrT chnd "Tx 2: "
              cPutStrLnBS chnd hash2
              cPutStrT chnd "Exchange: "
              cPrint chnd val
              cPutStrT chnd "Slot / Timestamp: "
              cPrint chnd (swapTime' pr)
              cPutStrT chnd "Actual Transaction Time : "
              cPrint chnd tmstpUTC -- (swapTime'' pr)
              cPutStrT chnd "Time when Processed     : "
              cPrint chnd swtim
              -- Print the subject
              cPutStrT chnd "Subject: "
              cPutStrLn chnd $ createSubject asst
              -- forMaybe (return ((0,0), (0,0))) val $ \exc -> do
              cPutStrLn chnd "Exchange Rate: "
              writeExchangeMaybe chnd asset val

            -- Writing/creating the candle point
            -- (Probably not used much anymore)
            forMaybe2
              (Just val) -- val'
              (atomically $ cPutStrLn chnd "Couldn't make point.")
              $ \exc -> do
                let !ac   = excAsset exc
                    -- tmstp
                    !rts = fst $! rats
                    !amt = snd $! rats
                    !tk' = T.pack . ktTicker <$> asset
                    !tk2 = maybe "" T.pack (getAssetName ac)
                    !tk3 = maybe (T.decodeUtf8 (getAssetToken ac)) T.pack (getAssetName ac)
                    !tik = fromMaybe tk2 tk'
                    !tak = fromMaybe tk3 tk'
                    !tkz = T.unpack tik
                    !dec = maybe 0 ktDecimal asset
                    !prdIdx = getPeriodIndex $ snd tmstp
                    !ktAlt = fromMaybe (KnownToken tkz tkz Nothing 0) asset
                    !liq = (fromIntegral lqd) / (10 ^ dec)
                    fullTok = tak -- <> "/" <> T.decodeUtf8 (getAssetPolicy ac)
                    fullPol = T.decodeUtf8 (getAssetPolicy ac)
                    tokPool = tak <> "/ADA"
                    adaLiq' = T.pack $ showFFloat (Just 6) (fromIntegral adaLiq / (10 ^ 6)) ""
                    poolPol = T.decodeUtf8 (getAssetPolicy poolAsset)
                    poolTok = T.decodeUtf8 (getAssetToken  poolAsset)
                    poolId  = poolTok <> "/" <> poolPol
  
                let cp = CandlePoint
                      ac
                      tmstp
                      prdIdx
                      tik
                      (fst $! rts)
                      (snd $! rts)
                      (exType exc)
                      hash2
                      "SUNDAE"
                      (fst amt)
                      (snd amt)
                      (fromIntegral lqd / (10 ^ dec))
                      Nothing -- ltk
  
                when candleChoice $ atomically $ addPoint pntMap cp

                atomically $ do
                  cPutStrT   chnd "Token Liquidity in Dex: "
                  cPutStrLn  chnd $ showFFloat (Just $ fromIntegral dec) (fromIntegral lqd / (10 ^ dec))
                                      (T.unpack $ " " <> tik)
                  -- Ada liquidity
                  cPutStrT    chnd "ADA Liquidity in Dex: "
                  cPutStrLnT  chnd $ adaLiq' <> " ADA"
                  cPutStrT    chnd "Current Period: "
                  cPrint      chnd prdIdx
                  cPutStrT    chnd "Pool Policy: "
                  cPutStrLnBS chnd $ getAssetPolicy poolAsset
                  cPutStrT    chnd "Pool Token: "
                  cPutStrLnBS chnd $ getAssetToken poolAsset
                
                -- Uses hsh1 instead of hash2 since
                -- hs1 can be used as the identifier for
                -- the entry.
                let dbx = DBExchange
                     (exType exc)
                     (snd amt)
                     (fst rts)
                     (fst amt)
                     hsh1 -- hash2
                     (snd $! swapTime' pr)
                     walt
                     -- (getAssetPolicy ac)
                     -- (getAssetToken  ac)
                     fullPol
                     fullTok
                     tokPool
                     poolId
                     Nothing
  
                -- Actual database stuff.
                -- Some transactions have 0 ADA. Ignore those.
                dbRslt <- unless' (0 == (fst amt)) $ accessCatch mSetup (insertMarket supplyCache "SUNDAE" ktAlt ac dbx liq adaLiq')
                atomically $ do
                  cPutStrLnT chnd "Database Result : "
                  cPrint     chnd dbRslt

writeExchangeMaybe :: ConHandle -> Maybe KnownToken -> Exchange -> STM ((Double, Double), (Double, Double))
writeExchangeMaybe chnd Nothing (ExBuy  ac tokAmt adaAmt) = writeExchange chnd ac tokAmt adaAmt
writeExchangeMaybe chnd Nothing (ExSell ac tokAmt adaAmt) = writeExchange chnd ac tokAmt adaAmt
writeExchangeMaybe chnd (Just kt) exc  = writeExchangeKnown chnd kt (excAsset exc) exc
{-# SCC writeExchangeMaybe #-}

unless' :: (Applicative f) => Bool -> f a -> f (Maybe a)
unless' p s =  if p then pure Nothing else (Just <$> s)

--       x Ada = y   Tokens
-- ==>   1 Ada = y/x Tokens
-- ==> x/y Ada = 1   Token
writeExchange :: ConHandle -> AssetClass -> Integer -> Integer -> STM ((Double, Double), (Double, Double))
writeExchange chnd ac tokAmt adaAmt = do
  let adaAmt' = fromIntegral @_ @Double adaAmt / 1_000_000
      lovAmt' = fromIntegral @_ @Double adaAmt
      tokAmt' = fromIntegral @_ @Double tokAmt
      adaPtok = adaAmt' / tokAmt'
      tokPada = tokAmt' / adaAmt'
      lovPtok = lovAmt' / tokAmt'
      tokPlov = tokAmt' / lovAmt'
      mTokenN = getAssetName ac
  forMaybe2
    mTokenN
    (cPutStrLn chnd $ "For Token " ++ show ac)
    (\tokN -> cPutStrLn chnd $ "For Token " ++ tokN ++ " of policy " ++ show (getAssetPolicy ac))
  -- cPutStrLn chnd $ "For Token " ++ show ac
  if tokPada > 100_000
    then do
      cPutStrLn chnd $ "1 Lovelace = " ++ showFFloat Nothing tokPlov " tokens"
      cPutStrLn chnd $ "1 Token    = " ++ showFFloat Nothing lovPtok " Lovelace"
    else do
      cPutStrLn chnd $ "1 Ada   = " ++ showFFloat Nothing tokPada " tokens"
      cPutStrLn chnd $ "1 Token = " ++ showFFloat Nothing adaPtok " Ada"
  return ((adaPtok, tokPada), (adaAmt', tokAmt'))

writeExchangeKnown :: ConHandle -> KnownToken -> AssetClass -> Exchange -> STM ((Double, Double), (Double, Double))
writeExchangeKnown chnd kt ac exc {-tokAmt adaAmt-} = do
  let adaPtok = getAdaRate kt exc
      tokPada = getTokRate kt exc
      tokStuf = getAmtRate kt exc
      tokTick = ktTicker kt
      tokLeng = length tokTick
      len = max tokLeng 3
      adaTick :: String
      adaTick = "Ada"
  cPutStr chnd $ "For Token " ++ ktName kt
  case ktDescription kt of
    Nothing  -> cPutStrLn chnd ""
    Just dsc -> cPutStrLn chnd $ " - " ++ dsc
  cPutStr chnd $ printf "1 %-*s = %f %s\n" len adaTick tokPada tokTick
  cPutStr chnd $ printf "1 %-*s = %f %s\n" len tokTick adaPtok adaTick
  return ((adaPtok, tokPada),tokStuf)

tryLookup :: Manager -> SM.Map AssetClass KnownToken -> AssetClass -> IO (Maybe KnownToken)
tryLookup mgr mp ac = do
  kt <- atomically $ lookupToken mp ac
  case kt of
    Nothing -> do
      -- Lookup the token names online,
      -- and if found, add it to the map.
      rkt <- queryMetadataNewRetry mgr ac
      case rkt of
        -- Nothing  -> putStrLn "lookups failed." >> return Nothing
        Nothing -> return Nothing
        Just kt' -> do
          atomically $ SM.insert kt' ac mp
          -- putStrLn $ "Lookup success: " ++ show kt'
          return rkt
    Just _ -> return kt

-- | A version of `maybe` where the function
-- argument comes last, when you want to use
-- it like `forM`.
forMaybe :: forall (b :: Type) (a :: Type).
  b -> Maybe a -> (a -> b) -> b
forMaybe df mb f = maybe df f mb

-- | Another version of `maybe` with
-- re-arranged arguments.
forMaybe2 :: forall (b :: Type) (a :: Type).
  Maybe a -> b -> (a -> b) -> b
forMaybe2 mb df f = maybe df f mb

app :: TPQueue tq => ThreadManager -> SM.Map BS.ByteString PartialTransaction  -> tq FullTransaction -> TTime -> Application
app dummy tmm tbq txTime = serve api (server dummy tmm tbq txTime)

api :: Proxy API
api = Proxy

server :: TPQueue tq => ThreadManager -> SM.Map BS.ByteString PartialTransaction  -> tq FullTransaction -> TTime -> Server API
server dummy mp tbq txTime = uploadValue
  where
    uploadValue :: TxEntry -> Handler TxEntry
    uploadValue txe = do
      liftIO $ processEntry dummy mp tbq txTime txe
      return txe

-- To allow easy changes.
processEntry :: TPQueue tq => ThreadManager -> SM.Map BS.ByteString PartialTransaction -> tq FullTransaction -> TTime -> TxEntry -> IO ()
processEntry dummy mp tbq txTime txe@TxEntry {txContext = ctx, txVariant = var} = do
  -- putStrLn $ "Received " ++ show var ++ ":"
  -- putByteStrLn $ encodePayloadStrict $ txPayload txe
  let txh = txHash txe -- the Key for the map
      pld = txPayload txe
  -- Note that the presence check is done in this
  -- thread, rather than in the watcher thread, 
  -- since the insertion might occur before the
  -- check otherwise.
  bl <- atomically $ checkKey mp txh -- checks that the key is empty.
  when bl $ watchKey2 10_000 dummy tbq mp txh -- spawns a new thread.
  updateTTimeIO txTime (ctxTimestamp ctx) -- update the TTime.
  catch @ComponentException (atomically $ addEntry mp txe) print
{-# SCC processEntry #-}

putJSONLn :: (ToJSON a) => a -> IO ()
putJSONLn x = do
  BS.putStr $ BL.toStrict $ encode x
  putStrLn ""

putByteStrLn :: BS.ByteString -> IO ()
putByteStrLn bs = do
  BS.putStr bs
  putStrLn ""

encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict x = BL.toStrict $ encode x

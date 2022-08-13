module Dex.Candles.ThreadMap (
  ThreadMap,
  getCandleQueue,
  registerAssetThread,
  RegisterOutcome,
  runThreadMap,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever, forM_, when, void)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Dex.Candles.CandleMap
import Dex.Candles.PointMap
import Dex.Candles.Time
import Dex.Candles.Types
import Dex.Hex (fromBase16)
import Dex.STM.Time
import Dex.STM.TPQueue
import Dex.Types.AssetMap (AssetClass (AssetClass))
import Dex.Types.KnownToken (KnownToken(..))
import Focus             qualified as F
import StmContainers.Map qualified as SM
import System.IO (hPutStrLn, stderr)
import Dex.IO (ConSink, directFlushT)
import Dex.Async (ThreadManager)

-- | A map to handle threads for sorting and
-- aggregating candles for specific tokens.
type ThreadMap (tq :: Type -> Type) = SM.Map AssetClass (tq [CandlePoint], Maybe (Async ()))

-- | The Bool type is @Nothing@ if there is no
-- thread running yet. Otherwise, return the
-- thread as well.
getCandleQueue :: forall (tq :: Type -> Type). (TPQueue tq) =>
  ThreadMap tq -> AssetClass -> STM (tq [CandlePoint], Maybe (Async () ))
getCandleQueue tmap ac = do
  mtq <- SM.lookup ac tmap
  case mtq of
    Nothing -> do
      tq <- newQueue 100 -- arg is ignored if unbounded
      return (tq,Nothing)
    Just tq -> return tq

-- | What the outcome of the registering
-- process was.
data RegisterOutcome
  = RSuccess -- ^ Successfully Registered
  | RMissing -- ^ Token not Found
  | RPresent -- ^ A thread has already been registered.
  deriving stock (Show, Eq, Ord, Enum)

-- | Returns @RSuccess@ if thread was registered successfully,
-- @RPresent@ if a different thread has already been registered,
-- and @RMissing@ if the asset class is not yet in the map.
registerAssetThread :: forall (tq :: Type -> Type). (TPQueue tq) =>
  ThreadMap tq -> AssetClass -> Async () -> STM RegisterOutcome
registerAssetThread tmap ac thrd = do
  outcm <- SM.focus (F.accessAndAdjust view altr) ac tmap
  return $ fromMaybe RMissing outcm
  where
    -- The 'adjust' argument to F.accessAndAdjust
    altr :: (tq [CandlePoint], Maybe (Async ())) -> (tq [CandlePoint], Maybe (Async ()))
    altr (tq, Nothing) = (tq,Just thrd)
    altr (tq, x) = (tq, x)
    view :: forall (a :: Type). (tq [CandlePoint], Maybe a) -> RegisterOutcome
    view (_, Nothing) = RSuccess
    view (_, Just  _) = RPresent 

-- runThreadMap :: SM.Map AssetClass KnownToken -> 

-- | Run the thread map with all
-- the fun stuff that entails.
--
-- This should really be in `Dex.Candles.Managers`.
runThreadMap :: 
  forall (tq1 :: Type -> Type)
  (tq2 :: Type -> Type)
  (tq3 :: Type -> Type).
  (TPQueue tq1, TPQueue tq2, TPQueue tq3) =>
  ThreadManager
  -> ConSink
  -> SM.Map AssetClass KnownToken
  -> ThreadMap tq1
  -> tq2 [CandlePoint] -- written to
  -> tq3 (Either CandleError CandleStick) -- written to
  -> TTime
  -> Bool
  -> IO ()
runThreadMap _ _ _ _ _ _ _ False = return ()
runThreadMap dummy errSink ktMap thrMap cptQ cstQ txTime True = forever $ do
  cps <- atomically $ readQueue cptQ
  let cps' = groupPoints cps
  forM_ cps' $ \(ac,pts) -> do
    (cq, mthr) <- atomically $ getCandleQueue thrMap ac
    when (isNothing mthr) $ do
      mkt <- atomically $ SM.lookup ac ktMap
      tcfg <- case mkt of
        Nothing -> return $ TokenConfig 1 ac (fromMaybe "<temp>" $ tryTokenName ac)
        Just kt -> return $ TokenConfig 1 ac (T.pack $ ktTicker kt)
      asy <- assetHandleThreads dummy errSink cq cstQ txTime tcfg
      -- might want to do something in the 
      -- case of failure, like writing to
      -- stderr.
      rslt <- atomically $ registerAssetThread thrMap ac asy
      -- Cancel the new thread if one was already in use.
      -- (Maybe kinda unsafe?)
      when (rslt == RPresent) $ do
        cancel asy -- Cancel the NEW thread, not the already present one.
        -- This should happen extremely rarely, so we
        -- definitely want to know when it happens.
        -- hPutStrLn stderr $ "Cancelled extra thread for " ++ T.unpack (tcTicker tcfg)
        atomically $ directFlushT errSink $ "Cancelled extra thread for " <> tcTicker tcfg
    atomically $ writeQueue cq pts

-- To get the basic token name out of an AssetClass.
tryTokenName :: AssetClass -> Maybe T.Text
tryTokenName (AssetClass tok _) =
  T.pack <$> fromBase16 tok



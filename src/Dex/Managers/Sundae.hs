module Dex.Managers.Sundae (
  SwapPair(..),
  addPart1,
  addPart2,
  addPart1Timeout,
  swapTime',
  swapTime'',
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, unless, when)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Dex.Async (startThreadE, ThreadManager)
import Dex.STM
import Dex.STM.Alternative
import Dex.STM.TPQueue
import Dex.Types.Component
import Dex.Types.ExtraTransaction
import Dex.Types.FullTransaction
import Dex.Types.Transaction
import Dex.Types.TxInput
import Dex.Types.TxOutput (txoAddress, TxOutput (txoAmount))
import GHC.Generics (Generic)
import StmContainers.Map qualified as SM
import Dex.Types.Context (Timestamp)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data SwapPair = SwapPair
  { swapPart1 :: NonEmpty.NonEmpty ExtraTransaction
  , swapPart2 :: FullTransaction
  , swapTime  :: UTCTime
  } deriving (Show, Eq, Generic)

swapTime' :: SwapPair -> Timestamp
swapTime' = ftTimestamp . swapPart2

swapTime'' :: SwapPair -> UTCTime
swapTime'' = posixSecondsToUTCTime . snd . ftTimestamp . swapPart2

instance Hashable SwapPair

instance ToJSON SwapPair where
  toJSON spp = object
    [ "swap_part_1" .= swapPart1 spp
    , "swap_part_2" .= swapPart2 spp
    , "swap_time"   .= swapTime spp
    ]
  toEncoding spp = pairs
    (  "swap_part_1" .= swapPart1 spp
    <> "swap_part_2" .= swapPart2 spp
    <> "swap_time"   .= swapTime spp
    )

-- addPart1 :: SM.Map BS.ByteString SwapPair -> FullTransaction -> STM ()
-- | Adds the first part of a `SwapPair` to the map.
-- Only use this if you want the item to stay in the
-- map indefinitely until the matching part2 is found.
-- Otherwise, use `addPart1Timeout`.
addPart1 :: BS.ByteString -> SM.Map TxInput ExtraTransaction -> FullTransaction -> STM ()
addPart1 adr mp ftx = do
  let txh     = txtHash $ ftTransaction ftx
      txOuts  = ftOutputs ftx
      txOuts' = filter (\(CmOutput txo _ _ _) -> adr == txoAddress txo) txOuts
  forM_ txOuts' $ \cmTxo -> let txId = cmoTxId cmTxo in
    SM.insert (extr ftx txId) (TxInput txId txh) mp
  -- SM.insert ftx txh mp

-- | Lookup the first part by looking at the
-- txinputs of a part2.
addPart2 :: SM.Map TxInput ExtraTransaction -> FullTransaction -> UTCTime -> STM SwapPair
addPart2 mp ftx ctime = do
  let txHashes = ftInputs ftx
  -- Lookup the Txins in the supplied map
  ftxs <- catMaybes <$> forM txHashes (\txin -> do
    ftx' <- SM.lookup txin mp
    return ((,txin) <$> ftx')
    )
  -- Try again if there's no matching transaction.
  -- Also check that at least one txout
  -- is NOT 4.5 Ada.
  -- TODO: Wait a fixed amount of time to make
  -- sure that all part1 transactions have
  -- been added.
  unless (checkFtxs ftxs) retry

  -- Return and delete each transaction you found.
  xs <- forM ftxs $ \(ftx',txin) -> do
    SM.delete txin mp
    return ftx'
  
  return $ SwapPair (NonEmpty.fromList xs) ftx ctime
-- addPart2 :: SM.Map BS.ByteString FullTransaction -> FullTransaction -> STM (Maybe SwapPair)

-- | Check that the full transactions are correct
checkFtxs :: [(ExtraTransaction, TxInput)] -> Bool
checkFtxs [] = False
checkFtxs [(etx,txi@(TxInput tid _))] = True
  -- | (Just txo) <- xUtxo
  -- = txoAmount txo /= 4_500_000
  -- | otherwise = True
  where
    utxos = ftOutputs $ etxTrans etx 
    xUtxo = cmoTxOut <$> find (\x -> cmoTxId x == tid) utxos
checkFtxs txs = True -- for now


-- | Add the first part, but with a watcher that removes it
-- from the map after a certain delay. This is likely the
-- function you want to use.
addPart1Timeout :: BS.ByteString -> ThreadManager -> Int -> SM.Map TxInput ExtraTransaction -> FullTransaction -> IO ()
addPart1Timeout adr dummy tim mp ftx = do
  let txh     = txtHash $ ftTransaction ftx
      txOuts  = ftOutputs ftx
      txOuts' = filter (\(CmOutput txo _ _ _) -> adr == txoAddress txo) txOuts
  forM_ txOuts' $ \cmTxOut -> do
    let txId   = cmoTxId cmTxOut
        txInId = TxInput txId txh
    atomically $ SM.insert (extr ftx txId) txInId mp
    -- putStrLn "About to start (in forM_)"
    void $ startThreadE 
      dummy 
      (atomically $ SM.delete txInId mp) -- remove the item if an error rises.
      (race_ (atomically $ checkPresent mp txInId) (waitRemove tim mp txInId))

-- | Retries until the item is no longer present.
-- By racing this with `waitRemove`, that thread
-- is cancelled once the item in question is 
-- removed from the map, but this thread is
-- cancelled if the time runs out.
checkPresent :: forall (a :: Type) (b :: Type). (Eq a, Hashable a) => SM.Map a b -> a -> STM ()
checkPresent mp txh = do
  val <- SM.lookup txh mp
  -- Retry if the item is present, return if it isn't.
  maybe (return ()) (const retry) val

-- | Waits a specified amount of time,
-- then removes an item from the map.
waitRemove :: forall (a :: Type) (b :: Type). (Eq a, Hashable a) => Int -> SM.Map a b -> a -> IO ()
waitRemove tim mp txh = do
  threadDelay (tim * 1000)
  atomically $ SM.delete txh mp
  -- putStrLn "Removed a Swap item."


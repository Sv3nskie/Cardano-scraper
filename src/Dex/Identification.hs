module Dex.Identification (
  hasOutputTo,
  sortTransaction,
  sortTransactionSTM,
  sortTransactionList,
  sortTransactionListSTM,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List (find)
import Dex.Types.Component (cmoTxOut)
import Dex.Types.FullTransaction
import Dex.Types.Transaction
import Dex.Types.TxOutput
import Dex.STM.TPQueue

-- | Checks whether a `FullTransaction` has 
-- any outputs to a specific address.
hasOutputTo :: BS.ByteString -> FullTransaction -> Bool
hasOutputTo bs ft =
  any ((== bs) . txoAddress . cmoTxOut) (ftOutputs ft)

-- | Send a transaction to a specific queue
-- based on a function that decides. If the
-- result of the function is Nothing, the
-- transaction is just ignored.
sortTransaction :: forall (tq :: Type -> Type). 
  TPQueue tq => 
  (FullTransaction -> Maybe (tq FullTransaction)) -> 
  FullTransaction ->
  IO ()
sortTransaction sorter ft = do
  let mtq = sorter ft
  case mtq of
    Nothing   -> return ()
    (Just tq) -> atomically $ writeQueue tq ft

-- | Like `sortTransaction`, but functions in
-- the `STM` monad rather than the `IO` monad.
sortTransactionSTM :: forall (tq :: Type -> Type). 
  TPQueue tq => 
  (FullTransaction -> Maybe (tq FullTransaction)) -> 
  FullTransaction ->
  STM ()
sortTransactionSTM sorter ft = do
  let mtq = sorter ft
  case mtq of
    Nothing   -> return ()
    (Just tq) -> writeQueue tq ft

-- | A specialised version of `sortTransaction`
-- that just has a list of @(address, queue)@
-- pairs. If a transaction has any outputs to
-- address, it is forwarded to the
-- corresponding queue.
sortTransactionList :: forall (tq :: Type -> Type).
  TPQueue tq =>
  [(BS.ByteString, tq FullTransaction)] ->
  FullTransaction ->
  IO ()
sortTransactionList lst =
  sortTransaction (\ft -> snd <$> find (\(bs,_) -> hasOutputTo bs ft) lst)

-- | Like `sortTransactionList`, but in the `STM`
-- monad instead of the `IO` monad.
sortTransactionListSTM :: forall (tq :: Type -> Type).
  TPQueue tq =>
  [(BS.ByteString, tq FullTransaction)] ->
  FullTransaction ->
  STM ()
sortTransactionListSTM lst =
  sortTransactionSTM (\ft -> snd <$> find (\(bs,_) -> hasOutputTo bs ft) lst)

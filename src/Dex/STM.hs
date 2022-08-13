module Dex.STM (
  addEntry,
  watchKey,
  watchKey',
  watchKey2,
  checkKey,
) where
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Dex.Async (startThreadTimed, ThreadManager)
import Dex.STM.TPQueue
import Dex.Types.Component (txComponent, txComponentE, txComponentIO, txComponentSTM, txComponentUnsafe)
import Dex.Types.Entry (TxEntry(..), TxPayload(..), txHash)
import Dex.Types.FullTransaction (
  FullTransaction(..), 
  PartialTransaction(..), 
  addPayload,
  checkTxComplete,
  emptyTransaction,
  completeTransaction,
 )
import Focus (alter) 
import GHC.Natural (Natural)
import StmContainers.Map qualified as SM
import Control.Exception (toException, SomeException)

-- | A map that has a TxHash as a key, and a payload
-- as a value. Once the watcher for the transaction
-- determines that the transaction is complete, the
-- transaction is pushed onto a queue from which it
-- can be read for further processing.
addEntry :: SM.Map BS.ByteString PartialTransaction -> TxEntry -> STM ()
addEntry mmp ent = do
  let thisHash = txHash ent
  thisValu <- txComponentSTM ent
  insertWith 
    addPayload 
    (addPayload emptyTransaction) 
    thisHash 
    thisValu
    mmp
{-# SCC addEntry #-}

-- | Like insertWith, but where the element to
-- be combined isn't of the same type as the 
-- values of the map. To compensate, there 
-- needs to be a second function that converts
-- the input values to the map values.
insertWith :: forall a b key. (Hashable key, Eq key) => 
  (b -> a -> Maybe b) -> (a -> Maybe b) -> 
  key -> a -> SM.Map key b -> STM ()
insertWith op f key val =
  SM.focus (alter fnName) key
  where
    fnName :: Maybe b -> Maybe b
    fnName Nothing    = f val
    fnName (Just ptx) = op ptx val

-- | Wait until a transaction is complete, and then
-- push it onto a queue. Note that you'll have to
-- remove the key in the exit portion of a bracket.
watchKey :: TPQueue tq => tq FullTransaction -> SM.Map BS.ByteString PartialTransaction -> BS.ByteString -> STM ()
watchKey tbq mp key = do
  val <- SM.lookup key mp
  case val of
    -- If there's nothing there, try again.
    Nothing  -> retry -- return ()
    -- Check whether the tx is complete.
    Just ptx -> do 
      let !mtx = completeTransaction ptx
      case mtx of
        Nothing  -> retry
        Just ftx -> writeQueue tbq ftx

-- | Like `watchKey`, but meant to be run
-- with `Dex.Async.runThreads`.
watchKey' :: TPQueue tq => Int -> ThreadManager -> tq FullTransaction -> 
  SM.Map BS.ByteString PartialTransaction -> BS.ByteString -> IO ()
watchKey' tim dummy tbq mp key = void $ 
  startThreadTimed 
    tim 
    dummy 
    (atomically $ SM.delete key mp)
    (checkWatch tbq mp key)

-- | Like `watchKey`, but meant to be run
-- with `Dex.Async.runThreads`. Unlike
-- `watchKey'`, this one doesn't check
-- that the value isn't being watched.
-- Note that this shouldn't be a problem;
-- it's okay if there's more than one watcher,
-- but there is a problem if there __aren't
-- any__ watchers.
watchKey2 :: TPQueue tq => Int -> ThreadManager -> tq FullTransaction -> 
  SM.Map BS.ByteString PartialTransaction -> BS.ByteString -> IO ()
watchKey2 tim dummy tbq mp key = void $ 
  startThreadTimed 
    tim 
    dummy 
    (atomically $ SM.delete key mp)
    (atomically $ watchKey tbq mp key)

-- | Check that the key isn't being watched.
-- i.e. that `SM.lookup` key mp == `Nothing`.
checkKey :: SM.Map BS.ByteString PartialTransaction -> BS.ByteString -> STM Bool
checkKey mp key = do
  val <- SM.lookup key mp
  case val of
    Nothing -> return True
    Just _  -> return False

-- | Check that the key isn't being watched,
-- then start watching it.
checkWatch :: TPQueue tq => tq FullTransaction -> SM.Map BS.ByteString PartialTransaction -> BS.ByteString -> IO ()
checkWatch tbq mp key = do
  bl <- atomically $ checkKey mp key
  when bl $ atomically (watchKey tbq mp key)

{-
startThreadTimed ::
  forall (a :: Type) (b :: Type) (c :: Type). 
  Int -> Async a -> IO b -> IO c -> IO (Async (Maybe c))
-}


{-|
Module : STM.TPQueue
Description : A class for different types of STM queues.

This module just lets you use the same interface
to use different types of STM queues and channels.
This is mostly useful when you want to quickly 
change between different kinds of queues.

-}

module Dex.STM.TPQueue (
  TPQueue(..),
) where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (STM, orElse)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Dex.Monad (replicateWhile)
import GHC.Natural (Natural)

-- | A class for general use of STM Queues
-- and Channels.
class TPQueue (tq :: Type -> Type) where
  -- | Create a new Queue. 
  newQueue :: forall (a :: Type). Natural -> STM (tq a)
  -- | Read the next value from the queue.
  readQueue    :: forall (a :: Type). tq a -> STM a
  -- | A version of 'readQueue' which does not retry. Instead it
  -- returns @Nothing@ if no value is available.
  tryReadQueue :: forall (a :: Type). tq a -> STM (Maybe a)
  tryReadQueue tq = (Just <$> readQueue tq) `orElse` return Nothing
  -- | Get the next value from the Queue without removing it,
  -- retrying if the channel is empty.
  peekQueue    :: forall (a :: Type). tq a -> STM a
  -- | A version of 'peekQueue' which does not retry. Instead it
  -- returns @Nothing@ if no value is available.
  tryPeekQueue :: forall (a :: Type). tq a -> STM (Maybe a)
  tryPeekQueue tq = (Just <$> peekQueue tq) `orElse` return Nothing
  -- | Write a value to a Queue.
  writeQueue   :: forall (a :: Type). tq a -> a -> STM ()
  -- | Put a data item back onto a channel, where it will be the next item read.
  unGetQueue   :: forall (a :: Type). tq a -> a -> STM ()
  -- | Returns 'True' if the supplied Queue is empty.
  isEmptyQueue :: forall (a :: Type). tq a -> STM Bool
  isEmptyQueue tq = isNothing <$> tryPeekQueue tq
  -- | Returns 'True' if the supplied Queue is bounded and full.
  isFullQueue  :: forall (a :: Type). tq a -> STM Bool
  -- | Read the entire contents of a Queue into a list. This
  -- function should never retry. Note that this may only be
  -- efficient for Queues and not Channels.
  flushQueue   :: forall (a :: Type). tq a -> STM [a]
  flushQueue tq = replicateWhile (tryReadQueue tq)

-- flushQueue   :: forall (a :: Type). tq a -> STM [a]

instance TPQueue TQueue where
  newQueue     = const newTQueue
  readQueue    = readTQueue
  tryReadQueue = tryReadTQueue
  peekQueue    = peekTQueue
  tryPeekQueue = tryPeekTQueue
  flushQueue   = flushTQueue
  writeQueue   = writeTQueue
  unGetQueue   = unGetTQueue
  isEmptyQueue = isEmptyTQueue
  isFullQueue  = const $ return False

instance TPQueue TBQueue where
  newQueue     = newTBQueue
  readQueue    = readTBQueue
  tryReadQueue = tryReadTBQueue
  peekQueue    = peekTBQueue
  tryPeekQueue = tryPeekTBQueue
  flushQueue   = flushTBQueue
  writeQueue   = writeTBQueue
  unGetQueue   = unGetTBQueue
  isEmptyQueue = isEmptyTBQueue
  isFullQueue  = isFullTBQueue

instance TPQueue TChan where
  newQueue     = const newTChan
  readQueue    = readTChan
  tryReadQueue = tryReadTChan
  peekQueue    = peekTChan
  tryPeekQueue = tryPeekTChan
  writeQueue   = writeTChan
  unGetQueue   = unGetTChan
  isEmptyQueue = isEmptyTChan
  isFullQueue  = const $ return False




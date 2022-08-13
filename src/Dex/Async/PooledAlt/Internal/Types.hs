{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Dex.Async.PooledAlt.Internal.Types (
  -- * Thread Manager
  ThreadManager(..),
  runThreads,
  cancelAll,
  -- * Running Threads
  startThreadX,
  -- * Existentially-Quantified Asyncs
  AsyncX(..),
  cancelX,
  cancelXWith,
  cancelWithX,
  uninterruptibleCancelX,
  waitX,
  -- * Others
  -- fixAsyncWithUnmask,
  dummyTimed,
  race2,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (void)
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Kind (Type)
import ListT qualified as LT
import StmContainers.Set qualified as Set
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

-- | Existentially quantified Async threads,
-- for when you want to use multiple in the
-- same context (e.g. in a set).
data AsyncX = forall r. AsyncX (Async r)

-- | `cancel` but for `AsyncX`
cancelX :: AsyncX -> IO ()
cancelX (AsyncX asy) = cancel asy

-- | `cancelWith` but for `AsyncX`.
cancelXWith :: forall (e :: Type). Exception e => AsyncX -> e -> IO ()
cancelXWith (AsyncX asy) e = cancelWith asy e

-- | `cancelWith` but for `AsyncX` and
-- with a different order of arguments.
cancelWithX :: forall (e :: Type). Exception e => e -> AsyncX -> IO ()
cancelWithX e (AsyncX asy) = cancelWith asy e

uninterruptibleCancelX :: AsyncX -> IO ()
uninterruptibleCancelX (AsyncX asy) = uninterruptibleCancel asy

-- | Like `wait`, but for `AsyncX` and
-- ignores the result.
waitX :: AsyncX -> IO ()
waitX (AsyncX asy) = void $ wait asy

-- instance Show AsyncX where
--   show (AsyncX asy) = show asy

instance Hashable AsyncX where
  hash (AsyncX asy) = hash asy
  hashWithSalt x (AsyncX asy) = hashWithSalt x asy

instance Eq AsyncX where
  (AsyncX x) == (AsyncX y) = asyncThreadId x == asyncThreadId y

instance Ord AsyncX where
  compare (AsyncX x) (AsyncX y) = compare (asyncThreadId x) (asyncThreadId y)

-- | The thread manager, which handles the various
-- threads that are spawned.
data ThreadManager = ThreadManager
  { tmThreads :: Set.Set AsyncX
  } 

-- | Start a new thread, using the 
-- `ThreadManager` to handle it.
startThreadX :: 
  forall (c :: Type). 
  ThreadManager -> IO c -> IO (Async c)
startThreadX mgr@(ThreadManager thdSet) action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (atomically $ Set.delete this' thdSet)
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Cancel all threads handled by
-- a thread manager, and remove them
-- from the pool. Note that this
-- cancels the threads in parallel,
-- but still waits until all threads
-- have been cancelled before returning.
cancelAll :: ThreadManager -> IO ()
cancelAll (ThreadManager thdSet) = do
  -- hmm...
  mask_ $ do
    rslt <- atomically $ do
      thrds <- LT.toList $ Set.listT thdSet
      Set.reset thdSet
      return thrds
    -- Cancel the threads concurrently.
    mapConcurrently_ cancelX rslt

-- | Like `cancelAll`, but runs in parallel
-- and only removes threads once they're
-- finished. Also returns an `Async` for
-- the thread that's doing the cancelling.
cancelAllP :: ThreadManager -> IO (Async ())
cancelAllP mgr@(ThreadManager thdSet) = do
  rslt <- mask_ $ atomically $ LT.toList $ Set.listT thdSet
  startThreadX mgr $ mapConcurrently_ cancelX rslt

-- From async-extras
{-
fixAsyncWithUnmask :: forall (a :: Type). (Async a -> (forall b . IO b -> IO b) -> IO a) -> IO (Async a)
fixAsyncWithUnmask f = mdo 
    this <- asyncWithUnmask $ f this
    return this
-}

-- | Use a `ThreadManager` to be able
-- to start threads as needed, but still
-- have them be cancelled at the end.
runThreads :: forall (b :: Type). (ThreadManager -> IO b) -> IO b
runThreads = bracket (ThreadManager <$> Set.newIO) cancelAll

-- | Like `dummyThread`, but times out
-- after a specified duration in milliseconds.
dummyTimed :: Int -> IO ()
dummyTimed ms = threadDelay (ms * 1000)

-- | Like `race`, but only cares about
-- the result of the second action.
-- Redundant; see `Dex.Async.Pooled.Types.runWithTimeout`.
race2 :: forall (a :: Type) (b :: Type). IO a -> IO b -> IO (Maybe b)
race2 f1 f2 = do
  x <- race f1 f2
  return $ case x of
    Left  _ -> Nothing
    Right y -> Just y


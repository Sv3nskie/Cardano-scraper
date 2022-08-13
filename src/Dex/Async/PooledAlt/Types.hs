
module Dex.Async.PooledAlt.Types (
    
  -- * Thread Manager
  ThreadManager,
  runThreads,
  cancelAll,
  -- * Starting Threads
  startThread,
  startThreadX,
  startThreadE,
  startThreadC,
  startThreadSetup,
  startThreadSetupE,
  startThreadSetupC,
  -- * Timed Threads
  startThreadTimed,
  startThreadTimedE,
  startThreadTimedC,
  startThreadSetupTimedC,
  -- * Querying the Thread Manager
  getThreadCount,
  getThreadCountSTM,
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Dex.Async.PooledAlt.Internal.Types
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (void)
import Data.Kind (Type)
import ListT qualified as LT
import StmContainers.Set qualified as Set

-- | Start a new thread, using the 
-- `ThreadManager` to handle it.
-- This version includes an extra "closing"
-- action that is performed after the thread
-- has been removed from the pool.
startThread :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThread mgr@(ThreadManager thdSet) closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy
    

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. Like
-- `startThread`, this function also
-- includes a closing action, but it
-- only runs when the thread exits on
-- exception.
startThreadE :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThreadE mgr@(ThreadManager thdSet) closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. This
-- function is essentially a combo of
-- `startThread` and `startThreadE`,
-- which runs different closing actions
-- depending on whether it exited
-- successfully or on exception.
startThreadC :: 
  forall (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO b' -> IO c -> IO (Async c)
startThreadC mgr@(ThreadManager thdSet) closer onErr action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; onErr })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. This
-- version has a "setup" action that 
-- performs some work and returns a value
-- that is passed to both the main action,
-- as well as a closer action.
startThreadSetup :: 
  forall (a :: Type) (b :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadSetup mgr@(ThreadManager thdSet) setup closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; closer bolt })
      atomically $ Set.delete this' thdSet
      closer bolt
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it.
-- Like `startThreadSetup`, but only runs
-- the closer action when the main
-- action exits on exception.
startThreadSetupE :: 
  forall (a :: Type) (b :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadSetupE mgr@(ThreadManager thdSet) setup closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; closer bolt })
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. Like
-- `startThreadSetup`, but with different
-- closing actions to run depending on 
-- whether the main action exited
-- successfully or on exception.
startThreadSetupC :: 
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async c)
startThreadSetupC mgr@(ThreadManager thdSet) setup closer onErr action =
  mask_ $ do 
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; onErr bolt })
      atomically $ Set.delete this' thdSet
      closer bolt
      return rslt
    putMVar mv asy
    return asy

-- | Get the current number of running threads
-- that are handled by this `ThreadManager`.
-- Unlike the linked version, this is no
-- faster than the STM version.
getThreadCount :: ThreadManager -> IO Integer
getThreadCount (ThreadManager mgrSet) =
  fromIntegral <$> (atomically $ Set.size mgrSet)

-- | Get the current number of running threads
-- handled by the `ThreadManager`.
getThreadCountSTM :: ThreadManager -> STM Integer
getThreadCountSTM (ThreadManager mgrSet) =
  fromIntegral <$> Set.size mgrSet

-- | Tries to run a function, but with
-- a timeout. Note that this __does__
-- cancel the thread if it doesn't 
-- finish in time.
runWithTimeout :: Int -> IO a -> IO (Maybe a)
runWithTimeout tim action = do
  rslt <- race (dummyTimed tim) action
  return $ case rslt of
    Left  _ -> Nothing
    Right x -> Just x 

-- | Like `startThread`, but has a timeout
-- to automatically cancel the thread 
-- after a certain time.
startThreadTimed ::
  forall (b :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO c -> IO (Async (Maybe c))
startThreadTimed tim mgr@(ThreadManager thdSet) closer action = 
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask (runWithTimeout tim action) `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy

-- | Like `startThreadTimed`, but the closer is
-- only used in errors.
startThreadTimedE ::
  forall (b :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO c -> IO (Async (Maybe c))
startThreadTimedE tim mgr@(ThreadManager thdSet) closer action =
  mask_ $ do 
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask (runWithTimeout tim action) `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Like `startThreadTimedE`, but with separate
-- closers for success and exceptions.
startThreadTimedC ::
  forall (b :: Type) (b' :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO b' -> IO c -> IO (Async (Maybe c))
startThreadTimedC tim mgr@(ThreadManager thdSet) closer onErr action =
  mask_ $ do 
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask (runWithTimeout tim action) `onException` (do { atomically $ Set.delete this' thdSet ; onErr })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy

-- | Like `startThreadSetupC`, but with 
-- a specified timeout.
startThreadSetupTimedC ::
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type). 
  Int -> ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async (Maybe c))
startThreadSetupTimedC tim mgr@(ThreadManager thdSet) setup closer onErr action =
  mask_ $ do 
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (runWithTimeout tim (action bolt)) `onException` (do { atomically $ Set.delete this' thdSet ; onErr bolt })
      atomically $ Set.delete this' thdSet
      closer bolt
      return rslt
    putMVar mv asy
    return asy
  
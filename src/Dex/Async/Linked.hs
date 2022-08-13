{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module Dex.Async.Linked (
  -- * Handling Threads
  runThreads,
  ThreadManager,
  -- ** Starting general threads.
  startThread,
  startThreadE,
  startThreadC,
  startThreadSetup,
  startThreadSetupC,
  -- ** Starting Timed Threads
  startThreadTimed,
  startThreadTimedE,
  startThreadTimedC,
  startThreadSetupTimedC,
  -- * Lower-level Helpers
  bracketChoice,
  dummyThread,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
  Async,
  async,
  withAsync,  
  waitEither,
  linkOnly,
  race,
  wait,
 )
import Control.Concurrent.STM.TVar (
  newTVarIO,
  readTVar,
 )
import Control.Exception (
  bracket,
  bracket_,
  bracketOnError,
  mask,
  onException,
 )
import Control.Monad.STM ( 
  STM,
  atomically,
  check,
 )
import Data.Kind (Type)

-- Just a sample usage.
{-
exampleRunner :: IO ()
exampleRunenr = runThreads $ \dummy -> do
  asy1 <- startThread dummy act1 closer1
  asy2 <- startThread dummy act2 closer2
  rslt <- waitEither asy1 asy2
  print rslt
-}

type ThreadManager = Async ()

-- | Start a thread where any threads spawned
-- with "`startThread` dummy ..." will automatically
-- be cancelled when the main thread finishes.
-- (In theory) 
runThreads :: forall (b :: Type). (ThreadManager -> IO b) -> IO b
runThreads = withAsync dummyThread
-- runThreads actions =
--   withAsync dummyThread actions

-- | Start a thread together with an action
-- to perform when the thread finishes.
startThread :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThread dummy closer action = async $
  bracket_
    (linkOnly (const True) dummy)
    closer
    action
    
-- | Like `startThread`, but the closer is only used in errors.
startThreadE :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThreadE dummy closer action = async $
  bracketOnError
    (linkOnly (const True) dummy)
    (\_ -> closer)
    (\_ -> action)

-- | Like `startThread`, but with separate closers
-- for success and exceptions.
startThreadC :: 
  forall (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO b' -> IO c -> IO (Async c)
startThreadC dummy closer closerErr action = async $
  bracketChoice
    (linkOnly (const True) dummy)
    (\_ -> closer)
    (\_ -> closerErr)
    (\_ -> action)

-- | Very similar to `bracket`, but with 
-- automatic linking.
startThreadSetup ::
  forall (a :: Type) (b :: Type) (c :: Type).
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadSetup dummy setup closer action = async $
  bracket
    (do {linkOnly (const True) dummy ; setup})
    closer
    action

-- | Very similar to `bracketChoice`, but with 
-- automatic linking.
startThreadSetupC ::
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type).
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async c)
startThreadSetupC dummy setup closer closerErr action = async $
  bracketChoice
    (do {linkOnly (const True) dummy ; setup})
    closer
    closerErr
    action

-- | Like `startThread`, but has a timeout
-- to automatically cancel the thread 
-- after a certain time.
startThreadTimed ::
  forall (b :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO c -> IO (Async (Maybe c))
startThreadTimed tim dummy closer action = async $
  bracket_
    (linkOnly (const True) dummy)
    closer
    (runWithTimeout tim action)

-- | Like `startThreadTimed`, but the closer is
-- only used in errors.
startThreadTimedE ::
  forall (b :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO c -> IO (Async (Maybe c))
startThreadTimedE tim dummy closer action = async $
  bracketOnError
    (linkOnly (const True) dummy)
    (\_ -> closer)
    (\_ -> runWithTimeout tim action)

-- | Like `startThreadTimedE`, but with separate
-- closers for success and exceptions.
startThreadTimedC ::
  forall (b :: Type) (b' :: Type) (c :: Type). 
  Int -> ThreadManager -> IO b -> IO b' -> IO c -> IO (Async (Maybe c))
startThreadTimedC tim dummy closer closerErr action = async $
  bracketChoice
    (linkOnly (const True) dummy)
    (\_ -> closer)
    (\_ -> closerErr)
    (\_ -> runWithTimeout tim action)

-- | Like `startThreadSetupC`, but with 
-- a specified timeout.
startThreadSetupTimedC ::
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type). 
  Int -> ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async (Maybe c))
startThreadSetupTimedC tim dummy setup closer closerErr action = async $
  bracketChoice
    (do {linkOnly (const True) dummy ; setup})
    closer
    closerErr
    (runWithTimeout tim . action)


-- | The only purpose of this thread is
-- to be cancelled once the inner action
-- of a call finishes.
dummyThread :: IO ()
dummyThread = do
  tv <- newTVarIO False
  atomically $ do
    b <- readTVar tv
    check b

-- | Like `dummyThread`, but times out
-- after a specified duration in milliseconds.
dummyTimed :: Int -> IO ()
dummyTimed ms = threadDelay (ms * 1000)

-- | Like `wait`, but gives up after
-- some time. Note that this does __not__
-- cancel the `Async` being run.
waitWithTimeout :: Int -> Async a -> IO (Maybe a)
waitWithTimeout tim asy = do
  rslt <- race (dummyTimed tim) (wait asy)
  return $ case rslt of
    Left  _ -> Nothing
    Right x -> Just x 

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

-- | A combination of `Control.Exception.bracket` and 
-- `bracketOnError` that uses different closers when
-- when receiving an exception vs. ending normally.
bracketChoice 
  :: IO a         -- ^ Computation to run first
  -> (a -> IO b ) -- ^ Computation to run on successful exit.
  -> (a -> IO b') -- ^ Computation to run on error.
  -> (a -> IO c ) -- ^ Main Computation
  -> IO c
bracketChoice before after onErr thing =
  mask $ \restore -> do
    x <- before
    r <- restore (thing x) `onException` onErr x
    _ <- after x
    return r

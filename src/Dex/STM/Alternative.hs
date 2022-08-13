module Dex.STM.Alternative (
  unMaybeSTM,
  unNothingSTM,
  tryActions,
  retryActions,
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad

-- | Turn an `STM` function that returns
-- a `Maybe` into one that retries when
-- it would otherwise return a Nothing.
unMaybeSTM :: STM (Maybe a) -> STM a
unMaybeSTM act = do
  mx <- act
  maybe retry return mx

-- | Like `unMaybeSTM`, but still encapsulates
-- the result in a Maybe.
unNothingSTM :: STM (Maybe a) -> STM (Maybe a)
unNothingSTM act = do
  mx <- act
  case mx of
    Just _  -> return mx
    Nothing -> retry

-- | Try a list of actions and return the
-- first one that doesn't return Nothing. 
-- Doesn't retry if it fails.
tryActions :: [STM (Maybe a)] -> STM (Maybe a)
tryActions = foldr ((<|>) . unNothingSTM) (return Nothing)
-- tryActions acts = foldr (<|>) (return Nothing) (map unNothingSTM acts)

-- | Like `tryActions`, but retries if none
-- of the action succeed.
retryActions :: [STM (Maybe a)] -> STM a
retryActions = foldr ((<|>) . unMaybeSTM) retry


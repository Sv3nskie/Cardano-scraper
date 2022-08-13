module Dex.Monad (
  replicateWhile,
) where

import Control.Applicative
import Control.Monad

-- | @'replicateWhile' act@ performs the action @act@ until
-- it returns @Nothing@, and then returns the list of results:
--
-- ==== __Examples__
--
-- >>> import Control.Concurrent.STM
-- >>> do { tq <- newTQueueIO ; atomically $ mapM (writeTQueue tq) [1,2,10] ; atomically $ replicateWhile (tryReadTQueue tq) }
-- [1,2,10]
replicateWhile :: (Monad m) => m (Maybe a) -> m [a]
replicateWhile act = act >>= loop
  where
    loop Nothing  = pure []
    loop (Just x) = (x :) <$> (act >>= loop)

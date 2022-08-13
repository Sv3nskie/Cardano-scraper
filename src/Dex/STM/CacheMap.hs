module Dex.STM.CacheMap (
  CacheMap,
) where

import Control.Concurrent.STM
import Data.Kind (Type)
import StmContainers.Map qualified as SM

-- | A Map where keys are paired with 
-- frequency uses so that you can remove
-- keys that aren't frequently used.
type CacheMap (key :: Type) (val :: Type) 
  = SM.Map key (val, Int)

-- (Not yet implemented)





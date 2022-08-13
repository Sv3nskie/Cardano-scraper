module Dex.Database.SupplyCache (
  SupplyCache,
  updateSupplyCache,
  lookupSupplyCache,
) where

import Control.Concurrent.STM
import Dex.Types.AssetMap (AssetClass)
import StmContainers.Map qualified as SM

-- | A map that serves as a cache of token supplies.
type SupplyCache = SM.Map AssetClass Integer

-- | Update the total supply of a token.
updateSupplyCache :: SupplyCache -> AssetClass -> Integer -> STM ()
updateSupplyCache mp ac supply = SM.insert supply ac mp
{-# SCC updateSupplyCache #-}

-- | Try to lookup the total supply of a token.
lookupSupplyCache :: SupplyCache -> AssetClass -> STM (Maybe Integer)
lookupSupplyCache mp ac = SM.lookup ac mp





module Dex.Types.Exchange (
    Exchange(..),
    excAsset,
    excTokens,
    excAda,
    isBuy,
    isSell,
) where

import Dex.Types.AssetMap (AssetClass)
import GHC.Generics (Generic)

-- | A data type for an exchange of values.
-- The first `Integer` is the number of tokens,
-- the second is the value of the Ada.
data Exchange
  = ExBuy  AssetClass Integer Integer
  | ExSell AssetClass Integer Integer
  deriving stock (Show, Eq, Ord, Generic)

excAsset :: Exchange -> AssetClass
excAsset (ExBuy  ac _ _) = ac
excAsset (ExSell ac _ _) = ac

excTokens :: Exchange -> Integer
excTokens (ExBuy  _ tok _) = tok
excTokens (ExSell _ tok _) = tok

excAda :: Exchange -> Integer
excAda (ExBuy  _ _ ada) = ada
excAda (ExSell _ _ ada) = ada

isBuy :: Exchange -> Bool
isBuy ExBuy {} = True
isBuy _ = False

isSell :: Exchange -> Bool
isSell ExSell {} = True
isSell _ = False
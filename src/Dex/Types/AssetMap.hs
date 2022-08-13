module Dex.Types.AssetMap (
  AssetClass(..),
  AssetMap(..),
  getAssetToken,
  getAssetPolicy,
  insertAsset,
  lookupAsset,
  assetLookup,
  fromAssetList,
  getAssetClass,
) where


import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Dex.Types.TxOutput (TxAsset(..))
import GHC.Generics (Generic)

-- | The asset class as a single type.
data AssetClass = AssetClass BS.ByteString BS.ByteString
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable AssetClass 

getAssetToken :: AssetClass -> BS.ByteString
getAssetToken (AssetClass tok _) = tok

getAssetPolicy :: AssetClass -> BS.ByteString
getAssetPolicy (AssetClass _ pol) = pol

-- | A map of assets as a `Map.Map`.
newtype AssetMap = AssetMap {getAssetMap :: Map.Map AssetClass Integer}
  deriving stock (Show, Eq, Generic)

instance Semigroup AssetMap where
  (AssetMap m1) <> (AssetMap m2) = AssetMap $ Map.unionWith (+) m1 m2

instance Monoid AssetMap where
  mempty = AssetMap Map.empty

-- | Add a `TxAsset` to an `AssetMap`.
insertAsset :: AssetMap -> TxAsset -> AssetMap
insertAsset (AssetMap mp1) (TxAsset amt ast pol) = AssetMap $
  Map.insertWith (+) (AssetClass ast pol) amt mp1

lookupAsset :: AssetMap -> AssetClass -> Integer
lookupAsset (AssetMap mp) ac = fromMaybe 0 $
  Map.lookup ac mp

assetLookup :: AssetClass -> AssetMap -> Integer
assetLookup = flip lookupAsset

-- | Combine a list of `TxAsset`s into an asset map.
fromAssetList :: [TxAsset] -> AssetMap
fromAssetList = foldl' insertAsset mempty

getAssetClass :: TxAsset -> AssetClass
getAssetClass (TxAsset amt ast pol) = AssetClass ast pol

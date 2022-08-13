module Dex.Types.TxOutput (
  TxOutput(..),
  TxAsset(..),
) where

import Data.Aeson
import Data.ByteString    qualified as BS
import Data.Text.Encoding qualified as T
import Data.Hashable (Hashable)

import Dex.Parsers ( 
  (.:<), 
  (.:?<), 
  (>:), 
  encodeByteString,
 )
import GHC.Generics (Generic)

-- | Represents a TxOuput value from input.
data TxOutput = TxOutput
  { txoAddress :: BS.ByteString
  , txoAmount :: Integer
  , txoAssets :: [TxAsset]
  } deriving stock (Show, Eq, Generic)

instance Hashable TxOutput

instance FromJSON TxOutput where
  parseJSON = withObject "Output" $ \v -> TxOutput
    <$> v .:< encodeByteString >: "address"
    <*> v .: "amount"
    <*> v .: "assets"

instance ToJSON TxOutput where
  toJSON txo = object
    [ "address" .= T.decodeUtf8 (txoAddress txo)
    , "amount"  .= txoAmount txo
    , "assets"  .= txoAssets txo
    ]
  toEncoding txo = pairs
    (  "address" .= T.decodeUtf8 (txoAddress txo)
    <> "amount"  .= txoAmount txo
    <> "assets"  .= txoAssets txo
    )

------------------------------
-- The asset for TxOutput

-- | Represents an asset in the list
-- of a TxInput.
data TxAsset = TxAsset
  { astAmount :: Integer
  , astAsset  :: BS.ByteString
  , astPolicy :: BS.ByteString
  } deriving stock (Show, Eq, Generic)

instance Hashable TxAsset

instance FromJSON TxAsset where
  parseJSON = withObject "Assets" $ \v -> TxAsset
    <$> v .: "amount"
    <*> v .:< encodeByteString >: "asset"
    <*> v .:< encodeByteString >: "policy"

instance ToJSON TxAsset where
  toJSON txa = object
    [ "amount" .= astAmount txa
    , "asset"  .= T.decodeUtf8 (astAsset  txa)
    , "policy" .= T.decodeUtf8 (astPolicy txa)
    ]
  toEncoding txa = pairs
    (  "amount" .= astAmount txa
    <> "asset"  .= T.decodeUtf8 (astAsset  txa)
    <> "policy" .= T.decodeUtf8 (astPolicy txa)
    )









module Dex.Types.Mint (
  TxMint(..),
  mintAsset,
) where

import Data.Aeson
import Data.ByteString    qualified as BS
import Data.Text.Encoding qualified as T
import Data.Hashable (Hashable)
import Dex.Parsers ( 
  (.:<),
  (>:), 
  encodeByteString,
 )
import GHC.Generics (Generic)
import Dex.Types.AssetMap (AssetClass (AssetClass))

data TxMint = TxMint
  { txmPolicy   :: BS.ByteString
  , txmAsset    :: BS.ByteString
  , txmQuantity :: Integer
  } deriving (Show, Eq, Generic)

instance Hashable TxMint

instance FromJSON TxMint where
  parseJSON = withObject "Mint" $ \v -> TxMint
    <$> v .:< encodeByteString >: "policy"
    <*> v .:< encodeByteString >: "asset"
    <*> v .: "quantity"

instance ToJSON TxMint where
  toJSON mnt = object
    [ "policy"   .= T.decodeUtf8 (txmPolicy mnt)
    , "asset"    .= T.decodeUtf8 (txmAsset  mnt)
    , "quantity" .= txmQuantity mnt
    ]
  toEncoding mnt = pairs
    (  "policy"   .= T.decodeUtf8 (txmPolicy mnt)
    <> "asset"    .= T.decodeUtf8 (txmAsset  mnt)
    <> "quantity" .= txmQuantity mnt
    )

mintAsset :: TxMint -> AssetClass
mintAsset txm = AssetClass (txmAsset txm) (txmPolicy txm)

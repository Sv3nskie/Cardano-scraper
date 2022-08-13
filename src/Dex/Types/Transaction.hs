module Dex.Types.Transaction (
  TxTransaction(..),
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
import Dex.Types.Anything (Anything)
import GHC.Generics (Generic)

-- | Represents a transaction type.
data TxTransaction = TxTransaction
  { txtFee :: Integer
  , txtHash :: BS.ByteString
  , txtInputCount :: Integer
  , txtInputs :: Maybe Anything -- TEMP
  , txtMetadata :: Maybe Anything -- TEMP
  , txtMint :: Maybe Anything -- TEMP
  , txtMintCount :: Integer
  , txtNetworkId :: Maybe Integer
  , txtOutputCount :: Integer
  , txtOutputs :: Maybe Anything -- TEMP
  , txtTotalOutput :: Integer
  , txtTTL :: Maybe Integer
  , txtValidIntervalStart :: Maybe Integer -- Temp?
  } deriving stock (Show, Eq, Generic)

instance Hashable TxTransaction

instance FromJSON TxTransaction where
  parseJSON = withObject "Transaction" $ \v -> TxTransaction
    <$> v .:   "fee"
    <*> v .:<  encodeByteString >: "hash"
    <*> v .:   "input_count"
    <*> v .:?  "inputs"
    <*> v .:?  "metadata"
    <*> v .:?  "mint"
    <*> v .:   "mint_count"
    <*> v .:?  "network_id"
    <*> v .:   "output_count"
    <*> v .:?  "outputs"
    <*> v .:   "total_output"
    <*> v .:?  "ttl"
    <*> v .:?  "validity_interval_start"    

instance ToJSON TxTransaction where
  toJSON txt = object
    [ "fee"          .= txtFee txt
    , "hash"         .= T.decodeUtf8 (txtHash txt)
    , "input_count"  .= txtInputCount txt
    , "inputs"       .= txtInputs txt
    , "metadata"     .= txtMetadata txt
    , "mint"         .= txtMint txt
    , "mint_count"   .= txtMintCount txt
    , "network_id"   .= txtNetworkId txt
    , "output_count" .= txtOutputCount txt
    , "outputs"      .= txtOutputs txt
    , "total_output" .= txtTotalOutput txt
    , "ttl"          .= txtTTL txt
    , "validity_interval_start" .= txtValidIntervalStart txt
    ]
  toEncoding txt = pairs
    (  "fee"          .= txtFee txt
    <> "hash"         .= T.decodeUtf8 (txtHash txt)
    <> "input_count"  .= txtInputCount txt
    <> "inputs"       .= txtInputs txt
    <> "metadata"     .= txtMetadata txt
    <> "mint"         .= txtMint txt
    <> "mint_count"   .= txtMintCount txt
    <> "network_id"   .= txtNetworkId txt
    <> "output_count" .= txtOutputCount txt
    <> "outputs"      .= txtOutputs txt
    <> "total_output" .= txtTotalOutput txt
    <> "ttl"          .= txtTTL txt
    <> "validity_interval_start" .= txtValidIntervalStart txt
    )

module Dex.Types.Context (
  TxContext(..),
  Timestamp,
  ctxTime,
) where

import Data.Aeson
import Data.ByteString    qualified as BS
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (Hashable)
import Dex.Parsers ( 
  (.:<), 
  (.:?<), 
  (>:), 
  encodeByteString,
 )
import GHC.Generics (Generic)

-- | The context for a Transaction, 
-- TxInput, or TxOutput.
data TxContext = TxContext
  { ctxBlockHash :: BS.ByteString
  , ctxBlockNumber :: Integer
  , ctxCertificateIdx :: Maybe Integer -- ?
  , ctxInputIdx :: Maybe Integer
  , ctxOutputAddress :: Maybe BS.ByteString
  , ctxOutputIdx :: Maybe Integer
  , ctxSlot :: Integer
  , ctxTimestamp :: POSIXTime -- Temp
  , ctxTxHash :: BS.ByteString
  , ctxTxIdx :: Integer
  } deriving stock (Show, Eq, Generic)

instance Hashable TxContext

type Timestamp = (Integer, POSIXTime)

ctxTime :: TxContext -> Timestamp
ctxTime ctx = (ctxSlot ctx, ctxTimestamp ctx)

-- Note: The FromJSON instance for Integer does
-- a bounds check, so it's safe to use directly.
instance FromJSON TxContext where
  parseJSON = withObject "Context" $ \v -> TxContext
    <$> v .:<  encodeByteString >: "block_hash"
    <*> v .: "block_number"
    <*> v .: "certificate_idx"
    <*> v .: "input_idx"
    <*> v .:?< encodeByteString >: "output_address"
    <*> v .: "output_idx"
    <*> v .: "slot"
    <*> v .: "timestamp"
    <*> v .:<  encodeByteString >: "tx_hash"
    <*> v .: "tx_idx"

instance ToJSON TxContext where
  toJSON ctx = object
    [ "block_hash"      .= T.decodeUtf8 (ctxBlockHash ctx)
    , "block_number"    .= ctxBlockNumber ctx
    , "certificate_idx" .= ctxCertificateIdx ctx
    , "input_idx"       .= ctxInputIdx ctx
    , "output_address"  .= (T.decodeUtf8 <$> ctxOutputAddress ctx)
    , "output_idx"      .= ctxOutputIdx ctx
    , "slot"            .= ctxSlot ctx
    , "timestamp"       .= ctxTimestamp ctx
    , "tx_hash"         .= T.decodeUtf8 (ctxTxHash ctx)
    , "tx_idx"          .= ctxTxIdx ctx
    ]
  toEncoding ctx = pairs
    (  "block_hash"      .= T.decodeUtf8 (ctxBlockHash ctx)
    <> "block_number"    .= ctxBlockNumber ctx
    <> "certificate_idx" .= ctxCertificateIdx ctx
    <> "input_idx"       .= ctxInputIdx ctx
    <> "output_address"  .= (T.decodeUtf8 <$> ctxOutputAddress ctx)
    <> "output_idx"      .= ctxOutputIdx ctx
    <> "slot"            .= ctxSlot ctx
    <> "timestamp"       .= ctxTimestamp ctx
    <> "tx_hash"         .= T.decodeUtf8 (ctxTxHash ctx)
    <> "tx_idx"          .= ctxTxIdx ctx
    )

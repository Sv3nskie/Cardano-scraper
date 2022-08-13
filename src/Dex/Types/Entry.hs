module Dex.Types.Entry (
  TxEntry(..),
  txHash,
  TxPayload(..),
  encodePayload,
  encodePayloadStrict,
  Variant(..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Encoding (text)
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.Text.Encoding qualified as T
import Dex.Parsers ( 
  (.:?<), 
  (>:), 
  encodeByteString, 
 )
import Dex.Types.Context (TxContext, ctxTxHash)
import Dex.Types.Transaction (TxTransaction)
import Dex.Types.TxInput (TxInput)
import Dex.Types.TxOutput (TxOutput)
import GHC.Generics (Generic)
import Dex.Types.Mint (TxMint)

-- | The full entry from Oura. Includes
-- context, fingerprint, and payload.
data TxEntry = TxEntry
  { txContext :: TxContext
  , txFingerprint :: Maybe BS.ByteString
  , txPayload :: TxPayload
  , txVariant :: Variant
  } deriving stock (Show, Eq, Generic)

instance Hashable TxEntry

txHash :: TxEntry -> BS.ByteString
txHash = ctxTxHash . txContext

instance FromJSON TxEntry where
  parseJSON = withObject "Entry" $ \v -> do
    ctx <- v .: "context"
    fpt <- v .:?< encodeByteString >: "fingerprint"
    txi <- v .:? "tx_input"
    txo <- v .:? "tx_output"
    txm <- v .:? "mint"
    txt <- v .:? "transaction"
    var <- v .:? "variant"
    (pload,var') <- parsePayload txi txo txm txt var 
    return $ TxEntry ctx fpt pload var'
    
-- Check that the payload matches the variant field.
parsePayload :: Maybe TxInput -> Maybe TxOutput -> Maybe TxMint -> Maybe TxTransaction -> Maybe Variant -> Parser (TxPayload, Variant)
parsePayload (Just txi) Nothing    Nothing    Nothing    (Just TxInput')  = return (PlInput  txi, TxInput')
parsePayload (Just txi) Nothing    Nothing    Nothing    Nothing          = return (PlInput  txi, TxInput')
parsePayload Nothing    (Just txo) Nothing    Nothing    (Just TxOutput') = return (PlOutput txo, TxOutput')
parsePayload Nothing    (Just txo) Nothing    Nothing    Nothing          = return (PlOutput txo, TxOutput')
parsePayload Nothing    Nothing    (Just txm) Nothing    (Just TxMint')   = return (PlMint   txm, TxMint')
parsePayload Nothing    Nothing    (Just txm) Nothing    Nothing          = return (PlMint   txm, TxMint')
parsePayload Nothing    Nothing    Nothing    (Just txt) (Just TxTrans')  = return (PlTrans  txt, TxTrans')
parsePayload Nothing    Nothing    Nothing    (Just txt) Nothing          = return (PlTrans  txt, TxTrans')
parsePayload Nothing Nothing Nothing Nothing _ = fail "No payload detected."
parsePayload m1 m2 m3 m4 _
  | null lst       = fail "No payload detected."
  | length lst > 1 = fail "More than one payload detected."
  | otherwise      = fail "'variant' does not match payload."
  where lst = catMaybes [m1 $> (), m2 $> (), m3 $> (), m4 $> ()]
{-# SCC parsePayload #-}

instance ToJSON TxEntry where
  toJSON txe = object
    [ "context"     .= txContext txe
    , "fingerprint" .= (T.decodeUtf8 <$> txFingerprint txe)
    , case txPayload txe of
        (PlInput  inp) -> "tx_input"    .= inp
        (PlOutput otp) -> "tx_output"   .= otp
        (PlMint   mnt) -> "mint"        .= mnt
        (PlTrans  trs) -> "transaction" .= trs
    , "variant"     .= txVariant txe
    ]
  toEncoding txe = pairs
    (  "context"     .= txContext txe
    <> "fingerprint" .= (T.decodeUtf8 <$> txFingerprint txe)
    <> case txPayload txe of
         (PlInput  inp) -> "tx_input"    .= inp
         (PlOutput otp) -> "tx_output"   .= otp
         (PlMint   mnt) -> "mint"        .= mnt
         (PlTrans  trs) -> "transaction" .= trs
    <> "variant"     .= txVariant txe
    )

-- | The actual payload for the entry.
-- Doesn't have `ToJSON`/`FromJSON`
-- instances since `TxEntry` encodes/decodes
-- the payload by itself.
data TxPayload
  = PlInput TxInput
  | PlOutput TxOutput
  | PlMint  TxMint
  | PlTrans TxTransaction
  deriving stock (Show, Eq, Generic)

instance Hashable TxPayload

-- | Convert a payload directly to a lazy
-- `BL.ByteString`. Useful for terminal output.
encodePayload :: TxPayload -> BL.ByteString
encodePayload (PlInput  x) = encode x
encodePayload (PlOutput x) = encode x
encodePayload (PlMint   x) = encode x
encodePayload (PlTrans  x) = encode x

-- | Convert a payload directly to a strict
-- `BS.ByteString`. Useful for terminal output.
encodePayloadStrict :: TxPayload -> BS.ByteString
encodePayloadStrict = BL.toStrict . encodePayload

-- | A simple type that tells what kind
-- of entry this is.
data Variant
  = TxInput'
  | TxOutput'
  | TxMint'
  | TxTrans'
  deriving stock (Eq, Generic)

instance Hashable Variant

instance Show Variant where
  show TxInput'  = "TxInput"
  show TxOutput' = "TxOutput"
  show TxMint'   = "Mint"
  show TxTrans'  = "Transaction"

instance FromJSON Variant where
  parseJSON = withText "Variant" $ \case
    "TxInput"     -> return TxInput'
    "TxOutput"    -> return TxOutput'
    "Mint"        -> return TxMint'
    "Transaction" -> return TxTrans'
    _             -> fail "String doesn't match"

instance ToJSON Variant where
  toJSON     TxInput'  = String "TxInput"
  toJSON     TxOutput' = String "TxOutput"
  toJSON     TxMint'   = String "Mint"
  toJSON     TxTrans'  = String "Transaction"
  toEncoding TxInput'  = text "TxInput"
  toEncoding TxOutput' = text "TxOutput"
  toEncoding TxMint'   = text "Mint"
  toEncoding TxTrans'  = text "Transaction"

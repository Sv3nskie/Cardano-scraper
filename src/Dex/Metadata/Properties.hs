module Dex.Metadata.Properties (
  MetaProp(..),
  getMetadataValue,
  Property(..)
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encoding qualified as Enc
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Dex.Parsers
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

data Property
   = Name'
   | Description'
   | Url'
   | Ticker'
   | Decimals'
   | Logo'
   deriving (Eq)

instance Show Property where
  show Name' = "name"
  show Description' = "description"
  show Url' = "url"
  show Ticker' = "ticker"
  show Decimals' = "decimals"
  show Logo' = "logo"

instance FromHttpApiData Property where
  parseUrlPiece "name"        = Right Name'
  parseUrlPiece "description" = Right Description'
  parseUrlPiece "url"         = Right Url'
  parseUrlPiece "ticker"      = Right Ticker'
  parseUrlPiece "decimals"    = Right Decimals'
  parseUrlPiece "logo"        = Right Logo'
  parseUrlPiece txt = Left $ "Couldn't parse property: " <> txt

instance ToHttpApiData Property where
  toUrlPiece Name'        = "name"
  toUrlPiece Description' = "description"
  toUrlPiece Url'         = "url"
  toUrlPiece Ticker'      = "ticker"
  toUrlPiece Decimals'    = "decimals"
  toUrlPiece Logo'        = "logo"

instance FromJSON Property where
  parseJSON = withText "property" $ \case
    "name"        -> return Name'
    "description" -> return Description'
    "url"         -> return Url'
    "ticker"      -> return Ticker'
    "decimals"    -> return Decimals'
    "logo"        -> return Logo'
    str -> fail $ "\"" ++ T.unpack str ++ "\" is not a property."

instance ToJSON Property where
  toJSON Name'        = String "name"
  toJSON Description' = String "description"
  toJSON Url'         = String "url"
  toJSON Ticker'      = String "ticker"
  toJSON Decimals'    = String "decimals"
  toJSON Logo'        = String "logo"
  toEncoding Name'        = Enc.text "name"
  toEncoding Description' = Enc.text "description"
  toEncoding Url'         = Enc.text "url"
  toEncoding Ticker'      = Enc.text "ticker"
  toEncoding Decimals'    = Enc.text "decimals"
  toEncoding Logo'        = Enc.text "logo"

-- | The container that the API returns
-- for most properties.
data MetaProp (a :: Type) = MetaProp
  { mpSignatures :: [MetaSig]
  , mpSequenceNumber :: Integer
  , mpValue :: a
  } deriving stock (Eq, Show, Generic, Functor)

instance Foldable MetaProp where
  foldMap f (MetaProp _ _ val) = f val

instance Traversable MetaProp where
  traverse f (MetaProp sig sqn val) = 
    MetaProp sig sqn <$> f val
    -- MetaProp <$> pure sig <*> pure sqn <*> f val

-- | Get the actual value out of a `MetaProp`.
getMetadataValue :: MetaProp a -> a
getMetadataValue (MetaProp _ _ val) = val

instance (FromJSON a) => FromJSON (MetaProp a) where
  parseJSON = withObject "Properties" $ \v -> MetaProp
    <$> v .: "signatures"
    <*> v .: "sequenceNumber"
    <*> v .: "value"

instance (ToJSON a) => ToJSON (MetaProp a) where
  toJSON mpr = object
    [ "signatures"     .= mpSignatures mpr
    , "sequenceNumber" .= mpSequenceNumber mpr
    , "value"          .= mpValue          mpr
    ]
  toEncoding mpr = pairs
    (  "signatures"     .= mpSignatures mpr
    <> "sequenceNumber" .= mpSequenceNumber mpr
    <> "value"          .= mpValue          mpr
    )

data MetaSig = MetaSig
  { sigPubKey :: BS.ByteString
  , sigSigned :: BS.ByteString
  } deriving stock (Show, Eq, Generic)

instance FromJSON MetaSig where
  parseJSON = withObject "Signature" $ \v -> MetaSig
    <$> v .:< encodeByteString >: "publicKey"
    <*> v .:< encodeByteString >: "signature"

instance ToJSON MetaSig where
  toJSON sig = object
    [ "publicKey" .= T.decodeUtf8 (sigPubKey sig)
    , "signature" .= T.decodeUtf8 (sigSigned sig)
    ]
  toEncoding sig = pairs
    (  "publicKey" .= T.decodeUtf8 (sigPubKey sig)
    <> "signature" .= T.decodeUtf8 (sigSigned sig)
    )

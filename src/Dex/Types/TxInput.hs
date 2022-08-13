module Dex.Types.TxInput (
  TxInput(..),
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

-- | The type of a TxInput value.
data TxInput = TxInput
  { txiIndex :: Integer
  , txiId :: BS.ByteString
  } deriving stock (Show, Eq, Generic)

instance Hashable TxInput

instance FromJSON TxInput where
  parseJSON = withObject "Input" $ \v -> TxInput
    <$> v .: "index"
    <*> v .:< encodeByteString >: "tx_id"

instance ToJSON TxInput where
  toJSON inp = object
    [ "index" .= txiIndex inp
    , "tx_id" .= T.decodeUtf8 (txiId inp)
    ]
  toEncoding inp = pairs
    (  "index" .= txiIndex inp
    <> "tx_id" .= T.decodeUtf8 (txiId inp)
    )

module Dex.Types.ExtraTransaction (
  ExtraTransaction(..),
  etxHash,
  extr,
) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Dex.Types.FullTransaction (FullTransaction(..))
import Dex.Types.Transaction (txtHash)
import GHC.Generics (Generic)

-- | A `FullTransaction` together with an
-- annotation for which transaction to pay
-- attention to.
data ExtraTransaction = ExtraTransaction
  { etxTrans :: FullTransaction
  , etxId :: Integer
  } deriving (Show, Eq, Generic)

instance Hashable ExtraTransaction

instance ToJSON ExtraTransaction where
  toJSON etx = object
    [ "transaction" .= etxTrans etx
    , "txout_id"    .= etxId etx
    ]
  toEncoding etx = pairs
    (  "transaction" .= etxTrans etx
    <> "txout_id"    .= etxId etx
    )

-- | Shorter synonym for `ExtraTransaction`.
extr :: FullTransaction -> Integer -> ExtraTransaction
extr = ExtraTransaction

etxHash :: ExtraTransaction -> BS.ByteString
etxHash = txtHash . ftTransaction . etxTrans
module Dex.Types.TokenSale (
  TokenSale(..),
  TokenSwap(..),
) where

import Data.Aeson
import Data.Hashable (Hashable)
import Dex.Types.TxOutput (TxAsset)
import GHC.Generics (Generic)

-- | A type indicating that a certain number
-- of tokens were sold for so many Ada.
data TokenSale = TokenSale
  { saleAsset :: TxAsset -- ^ Note that this contains the number of tokens.
  , saleValue :: Integer -- ^ The sale price, in Ada.
  } deriving stock (Show, Eq, Generic)

instance Hashable TokenSale

instance ToJSON TokenSale where
  toJSON ts = object
    [ "sale_asset" .= saleAsset ts
    , "sale_value" .= saleValue ts
    ]
  toEncoding ts = pairs
    (  "sale_asset" .= saleAsset ts
    <> "sale_value" .= saleValue ts
    )

-- | A type indicating that a certain number
-- of tokens were sold for so many of another
-- token.
data TokenSwap = TokenSwap
  { swapAsset :: TxAsset -- ^ Note that this contains the number of tokens.
  , swapValue :: TxAsset -- ^ The sale price, in Ada.
  } deriving stock (Show, Eq, Generic)

instance Hashable TokenSwap

instance ToJSON TokenSwap where
  toJSON ts = object
    [ "swap_asset" .= swapAsset ts
    , "swap_value" .= swapValue ts
    ]
  toEncoding ts = pairs
    (  "swap_asset" .= swapAsset ts
    <> "swap_value" .= swapValue ts
    )

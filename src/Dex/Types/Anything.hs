module Dex.Types.Anything (
  Anything, -- Don't export constructor
  fromAnything,
  toAnything,
) where

import Data.Aeson (
  decodeStrict,
  encode,
  object,
  FromJSON(parseJSON),
  ToJSON(toEncoding, toJSON),
 )
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Binary.Builder  qualified as BB
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- | A simple type that has a `FromJSON` instance
-- that just stores the JSON value as a bytestring. 
-- This is useful when you don't know what a field 
-- will be, but still want to store the data.
newtype Anything = Anything BS.ByteString
  deriving stock (Show, Eq, Generic)
  deriving newtype Hashable

instance FromJSON Anything where
  parseJSON v = return (Anything $ BL.toStrict $ encode v)

-- toEncoding directly outputs its
-- bytestring. To provide safety,
-- constructors aren't exported.
instance ToJSON Anything where
  toJSON (Anything x) = case decodeStrict x of
    Nothing  -> object []
    (Just v) -> v
  toEncoding (Anything x) = unsafeToEncoding (BB.fromByteString x)

-- | Get the `BS.ByteString` that is
-- stored in the `Anything`. This is very
-- useful in view-patterns, e.g.
--
--   > myFunction :: Handle -> Anything -> IO ()
--   > myFunction h (fromAnything -> bstr) =
--   >   hPut bstr
--
-- Since you can't write something like
--  
--   > myFunction :: ...
--   > myFunction h (Anything bstr) = ...
fromAnything :: Anything -> BS.ByteString
fromAnything (Anything x) = x

-- | Encode some data directly into
-- an Anything.
toAnything :: ToJSON a => a -> Anything
toAnything x = Anything $ BL.toStrict $ encode x

-- | Try to decode the `BS.ByteString` in
-- an `Anything` as a JSON value.
decodeAnything :: forall (a :: Type). (FromJSON a) => Anything -> Maybe a
decodeAnything (Anything bs) = decodeStrict bs

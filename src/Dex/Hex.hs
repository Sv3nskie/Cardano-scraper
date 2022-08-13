module Dex.Hex (
  showBase16,
  fromBase16,
  toBase16,
  createSubject,
  getAssetName,
) where

import Data.ByteString         qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8   qualified as C8
import Data.ByteString.Lazy    qualified as BL

import Data.Char
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Data.Word
import Dex.Types.AssetMap (AssetClass(AssetClass))
import Text.Printf

-- | Not actually required; strings are already
-- in base-16.
showBase16 :: BS.ByteString -> String
showBase16 = BS.foldr fld "" 
  where
    fld :: Word8 -> String -> String
    fld wd rst = printf "%02x" wd ++ rst

-- | For converting base-16 `BS.ByteString`s
-- to actual strings.
fromBase16 :: BS.ByteString -> Maybe String
fromBase16 bs
  | BS.null bs = Just ""
  | Just (c1,bs1) <- C8.uncons bs
  , Just (c2,bs2) <- C8.uncons bs1
  , isHexDigit c1
  , isHexDigit c2
  -- Swapping order since the string is big-endian.
  , d0  <-      digitToInt c2
  , d1  <- 16 * digitToInt c1
  , val <- chr $ d0 + d1
  , isPrint val
  = (val : ) <$> fromBase16 bs2
  | otherwise = Nothing

toBase16 :: T.Text -> BS.ByteString
toBase16 txt = 
  BL.toStrict $ BB.toLazyByteString $ foldMap BB.word8HexFixed (BS.unpack $ T.encodeUtf8 txt)

getAssetName :: AssetClass -> Maybe String
getAssetName (AssetClass tok _pol) = fromBase16 tok

-- | For running API requests.
createSubject :: AssetClass -> String
createSubject (AssetClass tok pol) =
  C8.unpack pol ++ C8.unpack tok

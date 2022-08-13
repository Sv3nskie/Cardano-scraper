module Dex.Metadata.Response (
  MetaResponse(..),
  MetaBatchResponse(..),
) where

import Control.Monad ((>=>),(<=<))
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Dex.Metadata.Properties
import Dex.Parsers

data MetaResponse = MetaResponse
  { mrSubject :: BS.ByteString
  , mrPolicy  :: Maybe BS.ByteString
  , mrName :: Maybe (MetaProp String)
  , mrDescription :: Maybe (MetaProp String)
  , mrUrl :: Maybe (MetaProp String)
  , mrTicker :: Maybe (MetaProp String)
  , mrDecimals :: MetaProp Integer
  , mrLogo :: Maybe (MetaProp BS.ByteString)
  } deriving stock (Show, Eq)

instance FromJSON MetaResponse where
  parseJSON = withObject "Response" $ \v -> MetaResponse
    <$> v .:<  encodeByteString >: "subject"
    <*> v .:?< encodeByteString >: "policy"
    <*> v .:? "name"
    <*> v .:? "description"
    <*> v .:? "url"
    <*> v .:? "ticker"
    <*> v .:? "decimals" .!= MetaProp [] 0 0
    <*> v .:?< (parseJSON @(MetaProp Value) >=> traverse encodeByteString) >: "logo"

-- parseJSON @(MetaProp Text) :: Value -> (Parser (MetaProp Text))
-- traverse :: (Trav t, Apply f) => (a -> f b) -> t a -> f (t b)
-- traverse :: (Value -> Parser ByteString) -> MetaProp Value -> Parser 

-- Maybe (MetaProp BS.ByteString)
-- fmap T.decodeUtf8

instance ToJSON MetaResponse where
  toJSON mer = object
    [ "subject"  .=  T.decodeUtf8    (mrSubject mer)
    , "policy"   .= (T.decodeUtf8 <$> mrPolicy  mer)
    , "name"     .= mrName mer
    , "description" .= mrDescription mer
    , "url"      .= mrUrl mer
    , "ticker"   .= mrTicker mer
    , "decimals" .= mrDecimals mer
    , "logo"     .= fmap (fmap T.decodeUtf8) (mrLogo mer)
    ]
  toEncoding mer = pairs
    (  "subject"  .=  T.decodeUtf8    (mrSubject mer)
    <> "policy"   .= (T.decodeUtf8 <$> mrPolicy  mer)
    <> "name"     .= mrName mer
    <> "description" .= mrDescription mer
    <> "url"      .= mrUrl mer
    <> "ticker"   .= mrTicker mer
    <> "decimals" .= mrDecimals mer
    <> "logo"     .= fmap (fmap T.decodeUtf8) (mrLogo mer)
    )

newtype MetaBatchResponse = MetaBatchResponse
  { mbrSubjects :: [MetaResponse]
  } deriving stock (Show, Eq)

instance FromJSON MetaBatchResponse where
  parseJSON = withObject "Batch Response" $ \v -> MetaBatchResponse
    <$> v .: "subjects"

instance ToJSON MetaBatchResponse where
  toJSON     (MetaBatchResponse subjs) = object [ "subjects" .= subjs ]
  toEncoding (MetaBatchResponse subjs) = pairs  ( "subjects" .= subjs )
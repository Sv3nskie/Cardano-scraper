module Dex.Metadata.Request (
  MetaRequest(..),
) where

import Data.Aeson
import Data.ByteString    qualified as BS
import Data.Text.Encoding qualified as T
import Dex.Hex (createSubject)
import Dex.Metadata.Properties
import Dex.Parsers
import Dex.Types.AssetMap (AssetClass(..))
import GHC.Generics (Generic)

-- | For when performing a batch request.
data MetaRequest = MetaRequest
  { mrqSubjects   :: [BS.ByteString]
  , mrqProperties :: [Property]
  } deriving (Show,Eq,Generic)

instance FromJSON MetaRequest where
  parseJSON = withObject "Request" $ \v -> MetaRequest
    <$> v .:< parseList encodeByteString >: "subjects"
    <*> v .: "properties"
  
instance ToJSON MetaRequest where
  toJSON mrq = object
    [ "subjects"   .= map T.decodeUtf8 (mrqSubjects mrq)
    , "properties" .= mrqProperties mrq
    ]
  toEncoding mrq = pairs
    (  "subjects"   .= map T.decodeUtf8 (mrqSubjects mrq)
    <> "properties" .= mrqProperties mrq
    )
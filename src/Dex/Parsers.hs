module Dex.Parsers (
  (.:<),
  (.:?<),
  (>:),
  (.:*),
  encodeByteString,
  parseList,
) where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson ( 
  (.!=), 
  (.:), 
  (.:?), 
  Key, 
  FromJSON, 
  Object, 
  Value, 
  withArray,
  withText,
 )
import Data.Aeson.Types ( 
  Parser, 
  explicitParseField, 
  explicitParseFieldMaybe, 
 )
import Data.ByteString    qualified as BS
import Data.Text.Encoding qualified as T
import Data.Vector        qualified as V

-- | Equivalent to `(.:)`, but with an
-- explicit parser. Use with `(>:)` like:
-- 
--   > v .:< parser >: "field"
(.:<) :: forall a. Object -> (Value -> Parser a) -> Key -> Parser a
(.:<) = flip explicitParseField

-- | Like `(.:<)`, but modifies the parser
-- to work on values that may be null or
-- not present.
(.:?<) :: forall a. Object -> (Value -> Parser a) -> Key -> Parser (Maybe a)
(.:?<) = flip explicitParseFieldMaybe

-- | Equivalent to `($)`, but looks nicer when
-- paired with `(.:<)` or any other similar
-- combinator.
(>:) :: forall a. (Key -> Parser a) -> Key -> Parser a
(>:) = ($)

-- | Parse one or several values. Succeeds if
-- the value is null, not present, a list, or
-- a single object.
(.:*) :: forall a. (FromJSON a) => Object -> Key -> Parser [a]
v .:* key = (v .:? key .!= []) <|> ((:[]) <$> v .: key)

-- | Get a bytestring that is encoded as a
-- UTF8 string.
encodeByteString :: Value -> Parser BS.ByteString
encodeByteString = withText "ByteString" (return . T.encodeUtf8)

parseList :: (Value -> Parser a) -> Value -> Parser [a]
parseList prs = withArray "List" $ \arr ->
  mapM prs $ V.toList arr

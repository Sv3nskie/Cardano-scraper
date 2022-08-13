module Dex.Types.Choice (
  Choice(..),
) where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson ( 
  FromJSON(parseJSON), 
  ToJSON(toEncoding, toJSON),
 )
import GHC.Generics (Generic)

-- | Like 'Either', but has a `FromJSON` instance
-- that just tries `Choice1`, then tries `Choice2`
-- if the first one failed. Useful as
-- ('Choice' a `Dex.Types.Anything.Anything`) if 
-- you want to use a simple fallback.
data Choice a b
  = Choice1 a
  | Choice2 b
  deriving stock (Show, Eq, Generic)

instance forall a b. (FromJSON a, FromJSON b) => FromJSON (Choice a b) where
  parseJSON v = (Choice1 <$> parseJSON @a v) <|> (Choice2 <$> parseJSON @b v)

instance forall a b. (ToJSON a, ToJSON b) => ToJSON (Choice a b) where
  toJSON (Choice1 x) = toJSON x
  toJSON (Choice2 y) = toJSON y
  toEncoding (Choice1 x) = toEncoding x
  toEncoding (Choice2 y) = toEncoding y

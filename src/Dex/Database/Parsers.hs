module Dex.Database.Parsers (
  (!??),
  (!?!),
  lookupRead,
  lookupHead,
  lookupHeadDoc,
) where

import Control.Applicative
import Data.Bson hiding (lookup)
import Data.Bson qualified as BSON
import Data.Kind (Type)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | A variant of `!?` of that also tries
-- to see if it's been encoded as a string.
(!??) :: forall (a :: Type). (Val a, Read a) => Document -> Label -> Maybe a
doc !?? lbl
  =   (doc !? lbl)
  <|> ((readMaybe @a) =<< (doc !? lbl :: Maybe String))

-- | Non-nested variant of `!??`.
(!?!) :: forall (a :: Type). (Val a, Read a) => Document -> Label -> Maybe a
doc !?! lbl
  =   BSON.lookup @a lbl doc
  <|> ((readMaybe @a) =<< BSON.lookup @String lbl doc)

-- | Non-inline version of `!?!`, but with
-- different order of arguments.
lookupRead :: forall (a :: Type). (Val a, Read a) => Label -> Document -> Maybe a
lookupRead = flip (!?!)

-- | Get the first element of an Array in a `Document`.
lookupHead :: Label -> Document -> Maybe Value
lookupHead lbl doc = do
  lst <- look lbl doc
  case lst of
    Array []     -> Nothing
    Array (x:xs) -> return x
    _            -> Nothing

lookupHeadDoc :: Label -> Document -> Maybe Document
lookupHeadDoc lbl doc = do
  val <- lookupHead lbl doc
  case val of
    (Doc doc) -> return doc
    _         -> Nothing


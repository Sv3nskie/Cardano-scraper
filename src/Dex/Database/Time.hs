module Dex.Database.Time (
  ptimeToVal,
  valToPtime,
  lookupTime,
) where

import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Bson
import Data.Int
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Convert a `POSIXTime` to an Int64 value.
ptimeToVal :: POSIXTime -> Value
ptimeToVal ptm = Int64 (floor ptm)

-- | Convert a BSON `Value` to a POSIXTime.
valToPtime :: Value -> Maybe POSIXTime
valToPtime (Int64 x) = Just $ fromIntegral x
valToPtime (Int32 x) = Just $ fromIntegral x
valToPtime (Float x) = Just $ realToFrac x
valToPtime (UTC tim) = Just $ utcTimeToPOSIXSeconds tim
valToPtime (Stamp (MongoStamp x)) = Just $ fromIntegral x
valToPtime (String txt) = fromIntegral <$> readMaybe @Int64 (T.unpack txt)
valToPtime _ = Nothing

-- | Lookup a `POSIXTime` in a `Document` using `valToPtime`.
lookupTime :: Label -> Document -> Maybe POSIXTime
lookupTime lbl doc = look lbl doc >>= valToPtime
module Dex.Database.Types.Exchange (
  DBExchange(..),
  dbxVal,
) where

import Data.Bson hiding (lookup)
import Data.Bson qualified as BSON
import Data.ByteString    qualified as BS
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX (POSIXTime)

import GHC.Generics (Generic)
import Dex.Candles.Types (ExType(..))
import Numeric (showFFloat)
import Text.Read (readMaybe)
import Dex.Database.Time (ptimeToVal, lookupTime)

-- | An entry in the market list.
data DBExchange = DBExchange
  { dbeType :: ExType
  , dbeTokenAmount  :: Double
  , dbeExchangeRate :: Double
  , dbeTotalPrice   :: Double
  , dbeTxHash       :: BS.ByteString
  , dbeTimestamp    :: POSIXTime
  , dbeWallet       :: BS.ByteString
  , dbeTokenPolicy  :: T.Text
  -- , dbeTokenName    :: BS.ByteString
  , dbeTokenName    :: T.Text
  , dbePoolId       :: T.Text
  , dbePoolToken    :: T.Text
  , dbePriceImpact  :: Maybe Double
  } deriving stock (Show, Eq, Generic)

instance Val DBExchange where
  val dbx = Doc $
    [ "buySell"         := String (convertExc $ dbeType dbx)
    , "amountTokens"    := String (T.pack $ showFFloat Nothing (dbeTokenAmount  dbx) "")
    , "pricePer"        := String (T.pack $ showFFloat Nothing (dbeExchangeRate dbx) "")
    , "priceTotal"      := String (T.pack $ showFFloat (Just 6) (dbeTotalPrice   dbx) "")
    , "transactionHash" := String (T.decodeUtf8 $ dbeTxHash dbx)
    , "timestamp"       := ptimeToVal (dbeTimestamp dbx)
    , "wallet"          := String (T.decodeUtf8 $ dbeWallet dbx)
    , "policy"          := String (dbeTokenPolicy dbx)
    , "symbol"          := String (dbeTokenName dbx)
    , "poolName"        := String (dbePoolId dbx)
    , "poolToken"       := String (dbePoolToken dbx)
    ] <> maybe [] (\pim -> ["priceImpact" := String (T.pack $ showFFloat Nothing pim "%")]) (dbePriceImpact dbx)
  cast' (Doc doc) = DBExchange 
    <$> (unvertExc =<< doc !? "buySell")
    <*> (readMaybe =<< doc !? "amountTokens")
    <*> (readMaybe =<< doc !? "pricePer")
    <*> (readMaybe =<< doc !? "priceTotal")
    <*> (T.encodeUtf8 <$> (doc !? "transactionHash"))
    <*> lookupTime "timestamp" doc
    <*> (T.encodeUtf8 <$> (doc !? "wallet"))
    <*> (doc !? "policy")
    <*> (doc !? "symbol")
    <*> (doc !? "poolName")
    <*> (doc !? "poolToken")
    <*> Just (readMaybe . stripPercent =<< doc !? "priceImpact") -- to remove the %
  cast' _ = Nothing

dbxVal :: Int -> DBExchange -> Value
dbxVal dec dbx = Doc $
  [ "buySell"         := String (convertExc $ dbeType dbx)
  , "amountTokens"    := String (T.pack $ showFFloat (Just dec) (dbeTokenAmount  dbx) "")
  , "pricePer"        := String (T.pack $ showFFloat Nothing    (dbeExchangeRate dbx) "")
  , "priceTotal"      := String (T.pack $ showFFloat (Just 6)   (dbeTotalPrice   dbx) "")
  , "transactionHash" := String (T.decodeUtf8 $ dbeTxHash dbx)
  , "timestamp"       := ptimeToVal (dbeTimestamp dbx)
  , "wallet"          := String (T.decodeUtf8 $ dbeWallet dbx)
  , "policy"          := String (dbeTokenPolicy dbx)
  , "symbol"          := String (dbeTokenName dbx)
  , "poolName"        := String (dbePoolId dbx)
  , "poolToken"       := String (dbePoolToken dbx)
  ] <> maybe [] (\pim -> ["priceImpact" := String (T.pack $ showFFloat Nothing pim "%")]) (dbePriceImpact dbx)

convertExc :: ExType -> T.Text
convertExc Buy  = "buy"
convertExc Sell = "sell"

unvertExc :: T.Text -> Maybe ExType
unvertExc "buy"  = Just Buy
unvertExc "sell" = Just Sell
unvertExc _ = Nothing

-- | Remove the percent symbol from
-- the end of a string.
stripPercent :: String -> String
stripPercent [] = []
stripPercent str
  | lst == '%' = frt
  | otherwise  = str
  where
    lst = last str
    frt = init str

{-
data DBExchange = DBExchange
  { dbeType :: ExType
  , dbeTokenAmount  :: Integer
  , dbeExchangeRate :: Double
  , dbeTotalPrice   :: Integer
  , dbeTxHash       :: BS.ByteString
  , dbeTimestamp    :: POSIXTime
  } deriving stock (Show, Eq, Generic)

-}

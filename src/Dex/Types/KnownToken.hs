{-# LANGUAGE ApplicativeDo #-}

module Dex.Types.KnownToken (
  KnownToken(..),
  MetaEntry(..),
  knownToken,
  getAdaRate,
  getTokRate,
  getAmtRate,
  lookupToken,
  addKnownToken,
  addAssetClass,
) where

import Control.Concurrent.STM
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Text.Encoding qualified as T
import Dex.Parsers
import Dex.Types.AssetMap
import Dex.Types.Exchange (Exchange(..))
import GHC.Generics (Generic)
import StmContainers.Map qualified as SM

-- | A token that we know some properties about,
-- e.g. How many decimal places it uses, its name,
-- etc...
data KnownToken = KnownToken
  { ktName        :: String  -- ^ The full name of the token.
  , ktTicker      :: String  -- ^ A short, "ticker-tape" name for the token.
  , ktDescription :: Maybe String -- ^ A description of the token
  , ktDecimal     :: Integer -- ^ Number of decimal points in token.
  } deriving stock (Show, Eq, Generic)

instance Hashable KnownToken

instance FromJSON KnownToken where
  parseJSON = withObject "KnownToken" $ \v -> KnownToken
    <$> v .:  "name"
    <*> v .:  "ticker"
    <*> v .:? "description"
    <*> v .:? "decimal" .!= 0

instance ToJSON KnownToken where
  toJSON kt = object
    [ "name"        .= ktName kt
    , "ticker"      .= ktTicker kt
    , "description" .= ktDescription kt
    , "decimal"     .= ktDecimal kt
    ]
  toEncoding kt = pairs
    (  "name"        .= ktName kt
    <> "ticker"      .= ktTicker kt
    <> "description" .= ktDescription kt
    <> "decimal"     .= ktDecimal kt
    )

-- | Smart constructor for `KnownToken`.
knownToken :: String -> Maybe String -> Maybe String -> Integer -> KnownToken
knownToken str (Just tik) dsc dec
  | dec <= 0  = KnownToken str tik dsc 0
  | otherwise = KnownToken str tik dsc dec
knownToken str Nothing dsc dec
  | dec <= 0  = KnownToken str str dsc 0
  | otherwise = KnownToken str str dsc dec

-- | Lookup a token in an `STM` `SM.Map`.
lookupToken :: SM.Map AssetClass KnownToken -> AssetClass -> STM (Maybe KnownToken)
lookupToken mp ac = SM.lookup ac mp

-- | Add a `KnownToken` directly to an `STM` `SM.Map`.
addKnownToken :: SM.Map AssetClass KnownToken -> AssetClass -> KnownToken -> STM ()
addKnownToken mp ac kt = SM.insert kt ac mp

-- | Add a known token to an `STM` `SM.Map`,
-- checking that the decimal counter is > 0.
addAssetClass :: SM.Map AssetClass KnownToken -> AssetClass -> String -> String -> Maybe String -> Integer -> STM ()
addAssetClass mp ac str tik dsc dec = SM.insert (knownToken str (Just tik) dsc dec) ac mp

-- | Get the amount of Ada required to buy 1 of this
-- token, taking into acount the decimal.
getAdaRate :: KnownToken -> Exchange -> Double
getAdaRate kt (ExBuy  ac tokN adaN) = getAdaRate' kt tokN adaN
getAdaRate kt (ExSell ac tokN adaN) = getAdaRate' kt tokN adaN

getAdaRate' :: KnownToken -> Integer -> Integer -> Double
getAdaRate' kt tokAmt adaAmt =
  let adaAmt' = fromIntegral @_ @Double adaAmt / 1_000_000
      tokAmt' = fromIntegral @_ @Double tokAmt / (10 ^ ktDecimal kt)
  in  adaAmt' / tokAmt'

-- | Get the number of tokens needed to buy 1 Ada.
getTokRate :: KnownToken -> Exchange -> Double
getTokRate kt (ExBuy  ac tokN adaN) = getTokRate' kt tokN adaN
getTokRate kt (ExSell ac tokN adaN) = getTokRate' kt tokN adaN

getTokRate' :: KnownToken -> Integer -> Integer -> Double
getTokRate' kt tokAmt adaAmt =
  let adaAmt' = fromIntegral @_ @Double adaAmt / 1_000_000
      tokAmt' = fromIntegral @_ @Double tokAmt / (10 ^ ktDecimal kt)
  in  tokAmt' / adaAmt'

-- | Get the amounts of tokens, decimalised.
getAmtRate :: KnownToken -> Exchange -> (Double, Double)
getAmtRate kt (ExBuy  ac tokN adaN) = getAmtRate' kt tokN adaN
getAmtRate kt (ExSell ac tokN adaN) = getAmtRate' kt tokN adaN

getAmtRate' :: KnownToken -> Integer -> Integer -> (Double, Double)
getAmtRate' kt tokAmt adaAmt = 
  let adaAmt' = fromIntegral @_ @Double adaAmt / 1_000_000
      tokAmt' = fromIntegral @_ @Double tokAmt / (10 ^ ktDecimal kt)
  in  (adaAmt', tokAmt')
  

-- | For storing the `KnownToken`s in a local file.
data MetaEntry = MetaEntry
  { meToken :: AssetClass
  , meData  :: KnownToken
  } deriving stock (Show, Eq, Generic)

instance FromJSON MetaEntry where
  parseJSON = withObject "MetaEntry" $ \v -> do
    tok <- v .:< encodeByteString >: "token"
    pol <- v .:< encodeByteString >: "policy"
    dat <- v .: "data"
    return $ MetaEntry (AssetClass tok pol) dat

instance ToJSON MetaEntry where
  toJSON mdat = object
    [ "token"  .= T.decodeUtf8 (getAssetToken  $ meToken mdat)
    , "policy" .= T.decodeUtf8 (getAssetPolicy $ meToken mdat)
    , "data"   .= meData mdat
    ]
  toEncoding mdat = pairs
    (  "token"  .= T.decodeUtf8 (getAssetToken  $ meToken mdat)
    <> "policy" .= T.decodeUtf8 (getAssetPolicy $ meToken mdat)
    <> "data"   .= meData mdat
    )
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Dex.Database.Actions (
  insertMarket,
  insertChart,
) where

import Control.Monad (void, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson qualified as BSON
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.MongoDB hiding (lookup)
import Dex.Candles.Types (CandleStick(..))
import Dex.Database.SupplyCache
import Dex.Database.Types.Exchange
import Dex.Database.Types.Candle
import Dex.Hex
import Dex.Types.AssetMap (AssetClass, getAssetPolicy, getAssetToken)
import Dex.Types.KnownToken
import Numeric
import Dex.Database.Parsers ((!?!), lookupRead, lookupHeadDoc)
import Control.Applicative (Applicative(liftA2), (<|>))
import Data.List (uncons)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class (MonadTrans(lift))
import System.IO ( hPutStrLn, stderr )
import Text.Read (readMaybe)
import Data.Time.Clock.POSIX (POSIXTime)
import Dex.Database.Time (valToPtime, ptimeToVal)

-- | Insert a market transaction, possibly including the
-- basic data if not yet in database.
insertMarket :: forall (m :: Type -> Type). (MonadIO m, MonadFail m) => 
  SupplyCache -> T.Text -> KnownToken -> AssetClass -> DBExchange -> Double -> T.Text -> Action m (Either Document Value)
insertMarket supplyCache dex kt ac dbx' liq adaLiq = do
  rawSup <- lookupTokenDB supplyCache ac
  let dc' :: Int
      dc'  = fromIntegral $ ktDecimal kt
      totSup :: Maybe Double
      totSup = (/ (10 ^ dc')) . fromIntegral <$> rawSup
      fSup :: Double -> String
      fSup x = showFFloat (Just dc') x ""
      showDub :: Double -> String
      showDub x = showFFloat Nothing x ""
      -- Show Liquidity with exact number of
      -- decimal places.
      !liq' = T.pack $ showFFloat (Just dc') liq ""
      addLiq :: Document -> Document
      addLiq docu = merge docu ["reserves" =: liq', "adaReserves" =: adaLiq]
      timStmp = ptimeToVal $ dbeTimestamp dbx'
      -- Converting the exchange to a value.
      dc@(Doc dbx) = dbxVal dc' dbx'
      addSup :: Document -> Document
      addSup flds = case totSup of
        Nothing  -> flds
        (Just n) -> 
          -- !?! probably not necessary here; still using
          -- it for safety.
          case ((flds !?! "price") :: Maybe Double) of
            Nothing   -> merge ["totalSupply" =: fSup n] flds
            (Just pr) -> merge ["totalSupply" =: fSup n, "marketcap" =: showDub (n * pr)] flds
  curs <- find (createLookup "pairs" dex exName subj)
  doc' <- next curs
  case doc' of
    Nothing -> do
      xrslt <- insert "pairs" (textifyDoc $ addSup
        [ "dex" =: dex
        , "version" := String "v1"
        , "pairName" := String exName
        , "pairAddress" := String subj
        , "decimals" =: ktDecimal kt
        , "symbol" := String tik
        , "stable" := Bool False
        , "token0" := String "ADA"
        , "token1" := String tok
        , "liquidity" := String liq'
        , "price" =: dbeExchangeRate dbx'
        -- , "market" := Array [dc]
        ])
      zrslt <- insert "transactions" (addLiq $ textifyDoc dbx)
      -- maybe return something from zrslt later.
      return $ Right xrslt
    (Just doc) -> do
      -- lookup the transactions, and sort
      -- by the most recent. Def need to check
      -- whether to use ascending or descending.
      -- I'm guessing use descending to get more
      -- recent transactions first.
      ncurs <- find ((createLookupTx @Query "transactions" (dbeTokenName dbx') (dbeTokenPolicy dbx') (dbePoolToken dbx') timStmp) {sort = ["timestamp" := Int32 (-1)]} )
      -- Note: Using Int32 in the above sort, since it's not actually a 
      -- timestamp value, but rather a flag for ascending/descending.
      -- Also, the example uses =:, but that doesn't seem to work here.
      -- prevTx <- lookupPrevious (dbeTimestamp dbx') ncurs -- next ncurs
      prevTx <- next ncurs
      let -- mrkt = BSON.look "market" doc
          -- len  = length <$> (cast @[Value] =<< mrkt)
          -- Looking up old price directly from the pair info
          oldPrc  = doc !?! "price"
          -- Looking up old price in the market history.
          -- oldPrc' = lookupRead @Double "pricePer" =<< lookupHeadDoc "market" doc
          oldPrc' = lookupRead @Double "pricePer" =<< prevTx
          !newPrc = dbeExchangeRate dbx'
          !prcIm  = do
            oldP <- oldPrc <|> oldPrc'
            return (100 * (newPrc - oldP) / oldP)
          prcImStr = (\x -> showFFloat Nothing x "%") <$> prcIm
          prcImTxt = maybe "<lookup issue>" T.pack prcImStr
          dbxNew' = dbx' {dbePriceImpact = prcIm}
          dcNew@(Doc dbxNew) = dbxVal dc' dbxNew'
          -- chrt = BSON.look @Maybe "chart" doc
          -- len' = length <$> (cast @[Value] =<< mrkt)
          -- docX = exclude ["chart"] (merge ["market" =: len, "priceImpact" := String prcImTxt] $ addSup doc)
          !dbxT = addLiq $ textifyDoc dbx
          docX = merge ["priceImpact" := String prcImTxt, "transaction" := Doc dbxT] $ addSup doc

      upsert (createLookup "pairs" dex exName subj) 
          (textifyDoc $ addSup (merge
            [ "liquidity" =: liq'
            , "price" =: dbeExchangeRate dbx'
            ] doc))
      insert "transactions" dbxT
      {- case mrkt of
        (Just (Array mkt)) -> do
          -- let mkt' = val dbxNew:take 49 mkt
          let mkt' = val dbxNew:mkt 
          -- inefficient but easy to do.
          -- will have to lookup how to do
          -- it with MongoDB properly.
              
          upsert (createLookup "pairs" dex exName subj) 
            (textifyDoc $ addSup (merge
              [ "market" := Array mkt'
              , "liquidity" =: liq'
              , "price" =: dbeExchangeRate dbx'
              ] doc))
        Just _  -> fail "\"market\" was not a list/array."
        Nothing -> upsert (createLookup "pairs" dex exName subj) 
          (textifyDoc $ addSup (merge
            [ "market" := Array [dcNew]
            , "liquidity" =: liq'
            , "price" =: dbeExchangeRate dbx'
            ] doc))
      -}
      return (Left docX)
  where 
    -- dc@(Doc dbx) = val dbx'
    tik = T.pack $ ktTicker kt
    tok = tik <> "/" <> T.decodeUtf8 (getAssetPolicy ac)
    exName = tik <> "/ADA"
    subj = T.pack $ createSubject ac

-- | Create a Selector for the given index.
createLookup :: (Select sel) => Collection -> T.Text -> T.Text -> T.Text -> sel
createLookup col dex pr subj = select
    [ "dex" =: dex
    , "pairName" =: pr
    , "pairAddress" =: subj -- Temp?
    ]
    col

-- | Create a lookup for charts.
createLookup' :: (Select sel) => Collection -> T.Text -> T.Text -> sel
createLookup' col dex subj = select
    [ "dex" =: dex
    , "pairAddress" =: subj -- Temp?
    ]
    col

-- | Create a lookup for the "transactions"
-- collection. 
createLookupTx :: (Select sel) => Collection -> T.Text -> T.Text -> T.Text -> Value -> sel
createLookupTx col token policy poolId tim = select
  [ "symbol"  =: token
  , "policy"  =: policy
  , "poolToken" =: poolId
  , "timestamp" =: ["$lte" := tim]
  ]
  col

tokenLookup :: (Select sel) => Collection -> AssetClass -> sel
tokenLookup col ac = select [ "tokenAddress" =: T.pack (createSubject ac) ] col

-- | Lookup the total supply of a token.
lookupTokenDB :: forall (m :: Type -> Type). (MonadIO m) => SupplyCache -> AssetClass -> Action m (Maybe Integer)
lookupTokenDB supplyCache ac = do
  curx <- find (tokenLookup "tokens" ac)
  tsp  <- next curx
  let oid = BSON.lookup @ObjectId @Maybe "_id" =<< tsp
      atv = BSON.lookup @Bool @Maybe "active"  =<< tsp
      nwr = BSON.lookup @Bool @Maybe "new"     =<< tsp
      subj = createSubject ac
      prs = concat $ BSON.lookup @[Document] @Maybe "pairs" =<< tsp
      lku = List.find (isJust . BSON.look "dex") prs
      stf = (,) <$> tsp <*> oid
  -- Only update if the 'active' field wasn't True or 'new' wasn't False.
  -- (or the pairs thing was empty)
  unless (atv == Just True && nwr == Just False && isJust lku) $ forMaybe stf (return ()) $ \(tsp',objid) -> do
    let pnam = (T.pack <$> getAssetName ac) <|> (BSON.lookup @T.Text @Maybe "symbol" =<< tsp)
        pnm' = fromMaybe "Token" pnam
        pdoc = [ "address"  := String (T.pack subj)
               , "version"  := String "V1"
               , "pairName" := String (pnm' <> "/ADA")
               , "dex"      := String "SUNDAE"
               ]
        tspZ = if isJust lku then [] else ["pairs" := Array (Doc pdoc : map Doc prs)]
        tspY = merge ["active" =: True, "new" =: False] tsp'
        tspX = merge tspZ tspY

    void $ upsert (select ["_id" := ObjId objid] "tokens") tspX
  let tspMain :: Maybe Integer
      tspMain = tsp >>= (!?! "totalSupply")
  -- Make it simpler 
  -- altLookup <- liftIO $ atomically $ lookupSupplyCache supplyCache ac
  -- maybe (return ()) (liftIO . atomically . updateSupplyCache supplyCache ac) tspMain
  case tspMain of
    Just sup -> do
      liftIO $ atomically $ updateSupplyCache supplyCache ac sup
      return $ Just sup
    Nothing ->
      liftIO $ atomically $ lookupSupplyCache supplyCache ac
  -- return (tspMain <|> altLookup)

-- Takes decimals into account, based on the database lookup.
{-# DEPRECATED lookupTokenDB' "Just use lookupTokenDB." #-}
lookupTokenDB' :: forall (m :: Type -> Type). (MonadIO m) => AssetClass -> Action m (Maybe Double)
lookupTokenDB' ac = do
  curx <- find (tokenLookup "tokens" ac)
  tsp  <- next curx
  let oid = BSON.lookup @ObjectId @Maybe "_id" =<< tsp
      atv = BSON.lookup @Bool @Maybe "active"  =<< tsp
      nwr = BSON.lookup @Bool @Maybe "new"     =<< tsp
      stf = (,) <$> tsp <*> oid
      dec :: Maybe Int
      dec = (!?! "decimals") =<< tsp
      totSup :: Maybe Integer
      totSup = (!?! "totalSupply") =<< tsp
  -- Only update if the 'active' field wasn't True or 'new' wasn't False.
  unless (atv == Just True && nwr == Just False) $ forMaybe stf (return ()) $ \(tsp',objid) -> do
    let tspX = merge ["active" =: True, "new" =: False] tsp'
    void $ upsert (select ["_id" := ObjId objid] "tokens") tspX
  return $ liftA2 (/) (fromIntegral <$> totSup) ((10 ^) <$> dec)

forMaybe :: Maybe a -> b -> (a -> b) -> b
forMaybe myb empt act = maybe empt act myb

-- | Convert any Number in a `Document` to a String.
textifyDoc :: Document -> Document
textifyDoc = map (fldMapL textifyNumbersZ)

-- | Convert any Number in a `Value` to a String.
textifyNumbers :: Value -> Value
textifyNumbers (Float x) = String $ T.pack $ showFFloat Nothing x ""
textifyNumbers (Int32 x) = String $ T.pack $ show x
textifyNumbers (Int64 x) = String $ T.pack $ show x
textifyNumbers (Array xs) = Array $ map textifyNumbers xs
textifyNumbers (Doc   xs) = Doc   $ map (fldMap textifyNumbers) xs
textifyNumbers x = x

textifyNumbersX :: Value -> Value
textifyNumbersX (Float x) = String $ T.pack $ showFFloat Nothing x ""
textifyNumbersX (Int32 x) = String $ T.pack $ show x
textifyNumbersX (Int64 x) = String $ T.pack $ show x
textifyNumbersX (Array xs) = Array $ map textifyNumbersX xs
textifyNumbersX (Doc   xs) = Doc   $ map (fldMapL textifyNumbersZ) xs
textifyNumbersX x = x

textifyNumbersY :: Value -> Value
textifyNumbersY vl@(String str) = maybe vl Int64 (readMaybe (T.unpack str))
textifyNumbersY x = x
-- May need to add more.

textifyNumbersZ :: Label -> Value -> Value
textifyNumbersZ "timestamp" = textifyNumbersY
textifyNumbersZ _           = textifyNumbersX

-- | Map over the value of a `Field`.
fldMap :: (Value -> Value) -> Field -> Field
fldMap f (lbl := val) = lbl := f val

-- | Variant of `fldMap`.
fldMapL :: (Label -> Value -> Value) -> Field -> Field
fldMapL f (lbl := val) = lbl := f lbl val


-- | Insert a candlestick into the database.
insertChart :: forall (m :: Type -> Type). (MonadIO m, MonadFail m) => SupplyCache -> T.Text -> CandleStick -> Int -> Action m (Either Document Value)
insertChart supplyCache dex cs decs = do
  rawSup <- lookupTokenDB supplyCache ac
  let totSup = (/ (10 ^ decs)) . fromIntegral <$> rawSup
      dbcand = convertDBCandle $ createDBCandle totSup cs
  curs <- find slx
  doc' <- next curs
  case doc' of
    Nothing -> 
      Right <$> insert "charts" (textifyDoc 
        [ "dex" =: dex
        , "version" := String "v1"
        , "pairAddress" := String subj
        , "chart" =: [dbcand]
        ])
    (Just doc) -> do
      let chrt' = BSON.look "chart" doc
          len   = length <$> (cast @[Value] =<< chrt')
          hed   = fmap fst . uncons =<< (cast @[Value] =<< chrt')
          -- mrkt = BSON.look @Maybe "market" doc 
          -- len' = length <$> (cast @[Value] =<< mrkt)
          docX  = merge ["chart" =: hed, "updatedChart" := dbcand] doc
      rsltDoc <- case chrt' of
        (Just (Array chrt)) -> do
          let chrt2 = val dbcand:chrt
              newDoc = textifyDoc $ merge ["chart" := Array chrt2] doc
          upsert slx' newDoc
          return newDoc
        Just _  -> fail "\"chart\" was not a list/array."
        Nothing -> do
          let newDoc = textifyDoc $ merge ["chart" := Array [dbcand]] doc
          upsert slx' newDoc
          return newDoc
      let chrtY = BSON.look "chart" rsltDoc
          hedY  = fmap fst . uncons =<< (cast @[Value] =<< chrtY)
          docY  = merge ["chart" =: hedY] rsltDoc
      return (Left [ "oldChart" =: docX, "newChart" =: docY])
  where
    ac = csAsset cs
    tik = csTicker cs
    subj = T.pack $ createSubject ac
    slx  = createLookup' "charts" dex subj
    slx' = createLookup' "charts" dex subj
    liq = csLiquidity cs


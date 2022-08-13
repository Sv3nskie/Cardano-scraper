{-# LANGUAGE ApplicativeDo #-}

module Dex.Metadata (
  queryMetadataNew,
  queryMetadataNewRetry,
  queryMetadata,
  queryMetadata',
  queryMetadataCS,
  queryMetadataCS',
) where

-- To check the Metadata through the API
-- at https://tokens.cardano.org/metadata/...

import Control.Exception
import Control.Monad (unless)
import Data.Aeson
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Dex.Hex (createSubject)
import Dex.IO
import Dex.Metadata.Properties
import Dex.Metadata.Response
import Dex.Metadata.Request (MetaRequest (MetaRequest))
import Dex.Types.AssetMap (AssetClass)
import Dex.Types.KnownToken
import GHC.Generics
import Network.HTTP.Client (
  newManager, 
  defaultManagerSettings, 
  responseStatus,
  Manager, 
  HttpException(..),
  HttpExceptionContent(..),
 )
import Network.HTTP.Types.Status
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import System.IO

-- | The Metadata API type.
type MAPI 
  =    "metadata" :> Capture "subject" Text :> Get '[JSON] MetaResponse
  :<|> "metadata" :> Capture "subject" Text :> "properties" :> Capture "property" Property :> Get '[JSON] MetaResponse
  :<|> "metadata" :> "query" :> ReqBody '[JSON] MetaRequest :> Post '[JSON] MetaBatchResponse

api :: Proxy MAPI
api = Proxy

getMetadata :: Text -> ClientM MetaResponse

getMetadataProp :: Text -> Property -> ClientM MetaResponse

getMetadataBatch :: MetaRequest -> ClientM MetaBatchResponse

getMetadata :<|> getMetadataProp :<|> getMetadataBatch = client api

{-
data MetaResponse = MetaResponse
  { mrSubject :: BS.ByteString
  , mrPolicy  :: Maybe BS.ByteString
  , mrName :: Maybe (MetaProp String)
  , mrDescription :: Maybe (MetaProp String)
  , mrUrl :: Maybe (MetaProp String)
  , mrTicker :: Maybe (MetaProp String)
  , mrDecimals :: Maybe (MetaProp Integer)
  , mrLogo :: Maybe (MetaProp BS.ByteString)
  }

getMetadataValue :: MetaProp a -> a
getMetadataValue (MetaProp _ _ val) = val
-}

-- :m + Data.Typeable Network.HTTP.Client Servant.Client Control.Exception

queryMetadataNew :: Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadataNew manager ac = do
  rslt <- runClientM (lookupAssetMetadataNew ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      hPutStrLn stderr $ "Received error: " ++ show ex
      return Nothing
    Right x -> return x

-- | Query for data, but retry in the event of a timeout
-- or similar, but don't do anything in the case of a 404.
queryMetadataNewRetry :: Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadataNewRetry = queryMetadataNewRetry' 15

-- | Retries until the counter reaches <= 0.
queryMetadataNewRetry' :: Int -> Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadataNewRetry' n _ _ | n <= 0 = return Nothing
queryMetadataNewRetry' n manager ac = do
  rslt <- runClientM (lookupAssetMetadataNew ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      case ex of
        ConnectionError se ->
          maybe 
            (do { hPutStrLn stderr $ "Received error: " ++ show ex ; return Nothing })
            (handleConnectionException n manager ac)
            (fromException @HttpException se)
        err -> do 
          hPutStrLn stderr $ "Received error: " ++ show err
          return Nothing
    Right x -> return x

-- | Choose what to do based on the kind of exception.
-- Can add more error handling later on for more types
-- of HTTP exceptions.
handleConnectionException :: Int -> Manager -> AssetClass -> HttpException -> IO (Maybe KnownToken)
handleConnectionException _ _ _ (InvalidUrlException url rsn) = do
  hPutStrLn stderr $ "Issue with url: " ++ url
  hPutStrLn stderr rsn
  return Nothing
handleConnectionException n manager ac (HttpExceptionRequest req ctx) = case ctx of
  (StatusCodeException rsp _) -> do
    let st = responseStatus rsp
    unless (st == status404) $ hPutStrLn stderr $ "Received response code " ++ show (statusCode st)
    return Nothing
  -- Error msg is temporary.
  ConnectionTimeout   -> hPutStrLn stderr "retry (time)" >> queryMetadataNewRetry' (n-1) manager ac
  ConnectionFailure _ -> hPutStrLn stderr "retry (fail)" >> queryMetadataNewRetry' (n-1) manager ac
  InternalException _ -> hPutStrLn stderr "retry (intr)" >> queryMetadataNewRetry' (n-1) manager ac
  _ -> do 
    hPutStrLn stderr "Received Error:"
    hPrint stderr ctx
    return Nothing

queryMetadata :: Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadata manager ac = do
  rslt <- runClientM (lookupAssetMetadata ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      hPutStrLn stderr $ "Received error: " ++ show ex
      return Nothing
    Right x -> return x

queryMetadata' :: Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadata' manager ac = do
  rslt <- runClientM (lookupAssetMetadata' ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      hPutStrLn stderr $ "Received error: " ++ show ex
      return Nothing
    Right x -> return x

queryMetadataCS :: ConSink -> Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadataCS csink manager ac = do
  rslt <- runClientM (lookupAssetMetadata ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      hPutStrLn stderr $ "Received error: " ++ show ex
      return Nothing
    Right x -> return x

queryMetadataCS' :: ConSink -> Manager -> AssetClass -> IO (Maybe KnownToken)
queryMetadataCS' csink manager ac = do
  rslt <- runClientM (lookupAssetMetadata' ac) $ mkClientEnv
    manager
    (BaseUrl Https "tokens.cardano.org" 443 "")
  case rslt of
    Left ex -> do
      hPutStrLn stderr $ "Received error: " ++ show ex
      return Nothing
    Right x -> return x

-- | Best version of this function. It only
-- generates one request and doesn't have
-- the logo in the payload.
lookupAssetMetadataNew :: AssetClass -> ClientM (Maybe KnownToken)
lookupAssetMetadataNew ac = do
  let txt = T.encodeUtf8 $ T.pack $ createSubject ac
      prp = [Name', Ticker', Description', Decimals']
  payload <- getMetadataBatch (MetaRequest [txt] prp)
  return $ do
    -- Might want to check that the 
    -- subject matches up...
    rslt <- listToMaybe $ mbrSubjects payload
    ktnm <- getMetadataValue <$> mrName rslt -- want to fail if this is missing
    let kdsc = getMetadataValue <$> mrDescription rslt
        ktik = getMetadataValue <$> mrTicker rslt
        kdec = getMetadataValue  $  mrDecimals rslt
    return $ knownToken ktnm ktik kdsc kdec -- very temp

{-
data MetaResponse = MetaResponse
  { mrSubject :: BS.ByteString
  , mrPolicy  :: Maybe BS.ByteString
  , mrName :: Maybe (MetaProp String)
  , mrDescription :: Maybe (MetaProp String)
  , mrUrl :: Maybe (MetaProp String)
  , mrTicker :: Maybe (MetaProp String)
  , mrDecimals :: MetaProp Integer
  , mrLogo :: Maybe (MetaProp BS.ByteString)
  }

-}


-- | Looks up the metadata by generating a response
-- for each property. This avoids downloading the
-- logo, which can be expensive.
lookupAssetMetadata :: AssetClass -> ClientM (Maybe KnownToken)
lookupAssetMetadata ac = do
  let txt = T.pack $ createSubject ac
  tokName <- fmap getMetadataValue . mrName        <$> getMetadataProp txt Name'
  tokTick <- fmap getMetadataValue . mrTicker      <$> getMetadataProp txt Ticker'
  tokDesc <- fmap getMetadataValue . mrDescription <$> getMetadataProp txt Description'
  tokDecs <-      getMetadataValue . mrDecimals    <$> getMetadataProp txt Decimals'
  return $ knownToken <$> tokName <*> pure tokTick <*> pure tokDesc <*> pure tokDecs

-- | Like `lookupAssetMetadata`, but generates one
-- response and looks up all its values from it.
lookupAssetMetadata' :: AssetClass -> ClientM (Maybe KnownToken)
lookupAssetMetadata' ac = do
  let txt = T.pack $ createSubject ac
  tokProp <- getMetadata txt
  return $ knownToken 
    <$>       (getMeta . mrName  )      tokProp
    <*> pure ((getMeta . mrTicker)      tokProp)
    <*> pure ((getMeta . mrDescription) tokProp)
    <*> pure (getMetadataValue $ mrDecimals tokProp)
  where
    getMeta :: Maybe (MetaProp a) -> Maybe a
    getMeta = fmap getMetadataValue


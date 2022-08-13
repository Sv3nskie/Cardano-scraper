module Dex.Database.Types.DatabaseToken (
  DBToken(..),
) where

import Control.Applicative ((<|>))
import Data.Bson hiding (lookup)
import Data.Bson qualified as BSON
import Data.Int (Int32)
import Data.ByteString    qualified as BS
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Dex.Hex
import Dex.Types.AssetMap (AssetClass(..))
import Dex.Types.KnownToken
import GHC.Generics (Generic)


-- | The type of a token to be stored in
-- the database.
data DBToken = DBToken
  { dbtAsset :: AssetClass
  , dbtInfo  :: KnownToken
  } deriving stock (Show, Eq, Generic)

-- dbtName :: DBToken -> Maybe T.Text
-- dbtName 

instance Val DBToken where
  val dbt = Doc
    [ "tokenAddress" := String (T.pack $ createSubject $ dbtAsset dbt)
    , "tokenName"    := String (T.pack $ ktName   $ dbtInfo dbt)
    , "symbol"       := String (T.pack $ ktTicker $ dbtInfo dbt)
    , "decimals"     := Int32 (fromIntegral $ ktDecimal $ dbtInfo dbt)
    ]
  cast' (Doc dbt) = do
    adr <- BSON.lookup @T.Text "tokenAddress" dbt
    nam <- BSON.lookup @T.Text "tokenName"    dbt
    dec <- BSON.lookup @Int32  "decimals"     dbt
    sym <- BSON.lookup @T.Text "symbol"       dbt
    let subj = T.encodeUtf8 adr
    ast <- splitSubject subj sym <|> splitSubject subj nam
    return (DBToken ast (KnownToken (T.unpack nam) (T.unpack sym) Nothing (fromIntegral dec)))
  cast' _ = Nothing
    
-- | Try to separate a subject into an `AssetClass`.
splitSubject :: BS.ByteString -> T.Text -> Maybe AssetClass
splitSubject bs tik
  -- If the TokenName is the same as the ticker...
  -- , tik' `BS.isSuffixOf` bs
  | (Just bs') <- BS.stripSuffix tik' bs
  = Just (AssetClass tik' bs')
  -- Maybe add other possibilities later...
  | otherwise = Nothing
  where tik' = toBase16 tik


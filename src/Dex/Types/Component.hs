{-|
Very similar to Payload, but with some of the context included
-}

module Dex.Types.Component (
  TxComponent(..),
  CmOutput(..),
  txComponent,
  txComponentE,
  txComponentIO,
  txComponentSTM,
  txComponentUnsafe,
  ComponentException(..),
) where

import Control.Exception (Exception)
import Control.Monad.STM (STM, throwSTM)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Dex.Types.Context
import Dex.Types.Entry
import Dex.Types.Transaction (TxTransaction)
import Dex.Types.TxInput (TxInput)
import Dex.Types.TxOutput (TxOutput)
import GHC.Generics (Generic)
-- import Dex.Types.Context (ctxOutputIdx)
import GHC.Stack (HasCallStack)
import Dex.Types.Mint (TxMint)

-- | Like `Dex.Types.Entry.TxPayload`, but with more
-- data from the context of the value.
data TxComponent
  = CmpTrans TxTransaction
  | CmpTxIn  TxInput
  | CmpTxOut CmOutput
  | CmpMint  TxMint
  deriving (Show,Eq, Generic)

instance Hashable TxComponent

instance ToJSON TxComponent where
  toJSON (CmpTrans x) = object [ "transaction" .= x ]
  toJSON (CmpTxIn  x) = object [ "tx_input"    .= x ]
  toJSON (CmpMint  x) = object [ "tx_mint"     .= x ]
  toJSON (CmpTxOut x) = toJSON x
  toEncoding (CmpTrans x) = pairs ( "transaction" .= x )
  toEncoding (CmpTxIn  x) = pairs ( "tx_input"    .= x )
  toEncoding (CmpMint  x) = pairs ( "tx_mint"     .= x )
  toEncoding (CmpTxOut x) = toEncoding x

-- | At the moment, just a `TxOutput` with an
-- additional @id@ field.
data CmOutput = CmOutput
  { cmoTxOut :: TxOutput
  , cmoTxId  :: Integer
  , cmoTime  :: Timestamp
  , cmoBlock :: Integer
  } deriving (Show, Eq, Generic)

instance Hashable CmOutput

instance ToJSON CmOutput where
  toJSON cmo = object
    [ "tx_output" .= cmoTxOut cmo
    , "tx_out_id" .= cmoTxId  cmo
    , "tx_slot"   .= fst (cmoTime cmo)
    , "tx_time"   .= snd (cmoTime cmo)
    , "tx_block"  .= cmoBlock cmo
    ]
  toEncoding cmo = pairs
    (  "tx_output" .= cmoTxOut cmo
    <> "tx_out_id" .= cmoTxId  cmo
    <> "tx_slot"   .= fst (cmoTime cmo)
    <> "tx_time"   .= snd (cmoTime cmo)
    <> "tx_block"  .= cmoBlock cmo
    )

-- | Get a `Component` out of a `TxEntry`. Returns
-- @Nothing@ if the @tx_out_id@ field is empty.
txComponent :: TxEntry -> Maybe TxComponent
txComponent txe = case txPayload txe of
   (PlTrans  tx) -> Just $ CmpTrans tx
   (PlInput  tx) -> Just $ CmpTxIn  tx
   -- (PlOutput tx) -> CmpTxOut . CmOutput tx <$> ctxInputIdx (txContext txe)
   -- equivalent to
   (PlMint   tx) -> Just $ CmpMint tx
   (PlOutput tx) -> case ctxOutputIdx (txContext txe) of
     Nothing  -> Nothing
     Just idx -> Just $ CmpTxOut $ CmOutput 
       tx
       idx
       (ctxTime (txContext txe))
       (ctxBlockNumber (txContext txe))

-- | Like `txComponent`, but returns an
-- @`Either` `ComponentException` `TxComponent`@ instead
-- of just @`Maybe` `TxComponent`@. The `ComponentException`
-- can then be thrown as desired.
txComponentE :: TxEntry -> Either ComponentException TxComponent
txComponentE txe = case txPayload txe of
  (PlTrans  tx) -> return $ CmpTrans tx
  (PlInput  tx) -> return $ CmpTxIn  tx
  (PlMint   tx) -> return $ CmpMint  tx
  (PlOutput tx) -> case ctxOutputIdx (txContext txe) of
    (Just tidx) -> return $ CmpTxOut $ CmOutput tx tidx (ctxTime $ txContext txe) (ctxBlockNumber $ txContext txe)
    Nothing     -> Left ComponentException -- "Error with txout_id"

-- | A version of `txComponent` that prints an
-- error to stdout if it could not obtain the
-- @output_idx@ field.
txComponentIO :: TxEntry -> IO (Maybe TxComponent)
txComponentIO txe = case txPayload txe of
  (PlTrans  tx) -> return $ Just $ CmpTrans tx
  (PlInput  tx) -> return $ Just $ CmpTxIn  tx
  (PlMint   tx) -> return $ Just $ CmpMint  tx
  (PlOutput tx) -> case ctxOutputIdx (txContext txe) of
    (Just tidx) -> return $ Just $ CmpTxOut $ CmOutput tx tidx (ctxTime $ txContext txe) (ctxBlockNumber $ txContext txe)
    Nothing     -> putStrLn "Error with txout_id" >> return Nothing

-- | A version of `txComponent` that runs in `STM`
-- and throws an error if it can't access the 
-- @output_idx@ field.
txComponentSTM :: TxEntry -> STM TxComponent
txComponentSTM txe = case txPayload txe of
  (PlTrans  tx) -> return $ CmpTrans tx
  (PlInput  tx) -> return $ CmpTxIn  tx
  (PlMint   tx) -> return $ CmpMint  tx
  (PlOutput tx) -> case ctxOutputIdx (txContext txe) of
    (Just tidx) -> return $ CmpTxOut $ CmOutput tx tidx (ctxTime $ txContext txe) (ctxBlockNumber $ txContext txe)
    Nothing     -> throwSTM ComponentException

-- | A partial version of `txComponent`.
txComponentUnsafe :: HasCallStack => TxEntry -> TxComponent
txComponentUnsafe txe = case txPayload txe of
  (PlTrans  tx) -> CmpTrans tx
  (PlInput  tx) -> CmpTxIn  tx
  (PlMint   tx) -> CmpMint  tx
  (PlOutput tx) -> CmpTxOut $ CmOutput tx (fromJust $ ctxOutputIdx $ txContext txe) (ctxTime $ txContext txe) (ctxBlockNumber $ txContext txe)

------------------------------
-- Exceptions

-- | An exception involving the creation
-- of a `TxComponent`.
data ComponentException =
  ComponentException
  deriving stock (Eq, Show)

instance Exception ComponentException

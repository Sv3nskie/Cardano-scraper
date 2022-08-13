module Dex.Types.FullTransaction (
  FullTransaction(..),
  PartialTransaction(..),
  combinePayloads,
  addPayload,
  emptyTransaction,
  checkTxComplete,
  completeTransaction,
) where

import Control.Monad (foldM)
import Data.Aeson
import Data.Hashable (Hashable)
import Dex.Types.Context (Timestamp, ctxTime)
import Dex.Types.Component (TxComponent(..), CmOutput, cmoTime, cmoBlock)
import Dex.Types.Entry (TxPayload(..))
import Dex.Types.Mint (TxMint)
import Dex.Types.Transaction (TxTransaction, txtInputCount, txtOutputCount, txtMintCount)
import Dex.Types.TxOutput (TxOutput)
import Dex.Types.TxInput (TxInput)
import GHC.Generics (Generic)

data FullTransaction = FullTransaction
  { ftTransaction :: TxTransaction
  , ftInputs      :: [TxInput]
  , ftOutputs     :: [CmOutput]
  , ftMinting     :: [TxMint]
  , ftTimestamp   :: Timestamp
  , ftBlockNumber :: Integer
  } deriving stock (Show, Eq, Generic)

instance Hashable FullTransaction

instance ToJSON FullTransaction where
  toJSON ftx = object
    [ "transaction" .= ftTransaction ftx
    , "tx_input"    .= ftInputs ftx
    , "tx_output"   .= ftOutputs ftx
    , "tx_slot"     .= fst (ftTimestamp ftx)
    , "tx_time"     .= snd (ftTimestamp ftx)
    ]
  toEncoding ftx = pairs
    (  "transaction" .= ftTransaction ftx
    <> "tx_input"    .= ftInputs ftx
    <> "tx_output"   .= ftOutputs ftx
    <> "tx_slot"     .= fst (ftTimestamp ftx)
    <> "tx_time"     .= snd (ftTimestamp ftx)
    )

-- For gradually building up a transaction.
data PartialTransaction = PartialTransaction
  { ptTransaction :: Maybe TxTransaction
  , ptInputs      :: [TxInput]
  , ptOutputs     :: [CmOutput]
  , ptMinting     :: [TxMint]
  , ptTimestamp   :: Maybe Timestamp
  , ptBlockNumber :: Maybe Integer
  } deriving stock (Show, Eq, Generic)

emptyTransaction :: PartialTransaction
emptyTransaction = PartialTransaction Nothing [] [] [] Nothing Nothing

addPayload :: PartialTransaction -> TxComponent -> Maybe PartialTransaction
addPayload ptx@(PartialTransaction {ptTransaction = ptt}) (CmpTrans ptr) =
  case ptt of
    Nothing -> Just $ ptx {ptTransaction = Just ptr}
    Just _  -> Nothing
addPayload ptx@(PartialTransaction {ptInputs = pti}) (CmpTxIn pin) =
  Just $ ptx {ptInputs = pin:pti}
addPayload ptx@(PartialTransaction {ptOutputs = pto, ptTimestamp = Nothing, ptBlockNumber = Nothing}) (CmpTxOut pot) =
  Just $ ptx {ptOutputs = pot:pto, ptTimestamp = Just (cmoTime pot), ptBlockNumber = Just (cmoBlock pot)}
addPayload ptx@(PartialTransaction {ptOutputs = pto, ptTimestamp = Nothing}) (CmpTxOut pot) =
  Just $ ptx {ptOutputs = pot:pto, ptTimestamp = Just (cmoTime pot)}
addPayload ptx@(PartialTransaction {ptOutputs = pto, ptBlockNumber = Nothing}) (CmpTxOut pot) =
  Just $ ptx {ptOutputs = pot:pto, ptBlockNumber = Just (cmoBlock pot)}
addPayload ptx@(PartialTransaction {ptOutputs = pto}) (CmpTxOut pot) =
  Just $ ptx {ptOutputs = pot:pto}
addPayload ptx@(PartialTransaction {ptMinting = ptm}) (CmpMint mnt) =
  Just $ ptx {ptMinting = mnt:ptm}
{-# SCC addPayload #-}

combinePayloads :: Foldable t => t TxComponent -> Maybe PartialTransaction
combinePayloads = foldM addPayload emptyTransaction

completeTransaction :: PartialTransaction -> Maybe FullTransaction
completeTransaction (PartialTransaction Nothing _ _ _ _ _) = Nothing
completeTransaction (PartialTransaction _ _ _ _ Nothing _) = Nothing
completeTransaction (PartialTransaction _ _ _ _ _ Nothing) = Nothing
completeTransaction (PartialTransaction (Just ptx) pins pouts pmints (Just tim) (Just blk))
  | pilen == pilen' && polen == polen' && pmlen == pmlen' = Just $ FullTransaction ptx pins pouts pmints tim blk
  | otherwise = Nothing
  where
    pilen  = fromIntegral $ length pins
    polen  = fromIntegral $ length pouts
    pmlen  = fromIntegral $ length pmints
    pilen' = txtInputCount  ptx
    polen' = txtOutputCount ptx
    pmlen' = txtMintCount   ptx

checkTxComplete :: PartialTransaction -> Bool
checkTxComplete (PartialTransaction Nothing _ _ _ _ _) = False
checkTxComplete (PartialTransaction _ _ _ _ Nothing _) = False
checkTxComplete (PartialTransaction _ _ _ _ _ Nothing) = False
checkTxComplete (PartialTransaction (Just ptx) pins pouts pmints (Just _) (Just _))
  = pilen == pilen' && polen == polen' && pmlen == pmlen'
  where
    pilen  = fromIntegral $ length pins
    polen  = fromIntegral $ length pouts
    pmlen  = fromIntegral $ length pmints
    pilen' = txtInputCount  ptx
    polen' = txtOutputCount ptx
    pmlen' = txtMintCount   ptx
{-# SCC checkTxComplete #-}
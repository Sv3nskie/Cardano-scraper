module Dex.Managers.Sundae.Calculator (
  calculatePrice,
  calculatePrice',
  getBlockNumber,
  getLiquidityToken,
  -- Exchange(..)
) where

import Control.Arrow ((&&&),(***), first, second)
import Data.ByteString qualified as BS
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Dex.Managers.Sundae (SwapPair(..))
import Dex.Types.AssetMap
import Dex.Types.Exchange (Exchange(..))
import Dex.Types.ExtraTransaction
import Dex.Types.FullTransaction
import Dex.Types.Mint
import Dex.Types.TokenSale
import Dex.Types.Component (cmoTxId, cmoTxOut, CmOutput)
import Dex.Types.TxOutput (TxAsset(TxAsset, astAmount), TxOutput (txoAddress, txoAssets, txoAmount))
import GHC.Generics (Generic)
import Dex.Types.Transaction (txtHash)

-- | The new way to calculate swaps. (sunAddr is the znu address)
calculatePrice' :: BS.ByteString -> BS.ByteString -> SwapPair -> Maybe ([(FullTransaction , Exchange, BS.ByteString)], Integer, AssetClass, Integer, AssetClass, Int, [FullTransaction])
calculatePrice' sunAddr1 sunAddr sp = do
  let !swap1 = swapPart1 sp
      !swap2 = swapPart2 sp

      -- swap2 Outputs
      !swap2Outputs  = ftOutputs swap2
      !swap2Outputs' = map cmoTxOut swap2Outputs

      -- Like tx1Infos, but for the outputs.
      !tx2Infos = map txOutToVP swap2Outputs'
      !tx2Addrs = map vpAddress tx2Infos

      -- Information about the input values, indexed by Wallet.
      -- Essentially, the inputs from the transaction.
      
      -- Old Version
      -- tx1Info' = mapMaybe (maybePair . (maybePair . ((Just . etxTrans) &&& guessWallet) &&& getEscrow sunAddr1)) (NonEmpty.toList swap1)
      -- New Version
      tx1Info' = mapMaybe (maybePair . (maybePair . ((Just . etxTrans) &&& guessWallet' tx2Addrs) &&& getEscrow sunAddr1)) (NonEmpty.toList swap1)
      
      !tx1Infos = map (\((hsh,adr),(amt,asts)) -> (hsh,ValuePair adr amt asts)) tx1Info'
      
      -- Looking up Minted Assets
      !mintAssets = map mintAsset (ftMinting swap2)
      

      -- Now, match up tx1Infos and tx2Infos by address
      -- to get the raw swaps. Hmm...

      -- First: Figure out the swap token.
      -- Most of the output to sunAddr will
      -- be the asset we are looking for.
  
  -- Since this should never fail, can put this
  -- on the top level of this do-notation.
  !sunOutput <- find (\txo -> txoAddress txo == sunAddr) swap2Outputs'
  
  -- Get the token and its liquidity
  !sunTokens <- find (\txa -> astAmount txa /= 1) (txoAssets sunOutput)
  !poolToken <- find (\txa -> astAmount txa == 1) (txoAssets sunOutput)

  let tokenClass  = getAssetClass sunTokens
      tokenLiq    = astAmount sunTokens
      adaReserves = txoAmount sunOutput
      poolAsset = getAssetClass poolToken

      -- (R)ejected or (A)ccepted
      (!tx1InfosR, !tx1InfosA) = findCopies tx1Infos

      -- Now, pair up tx1Infos with tx2Infos...
      (!unmatchedTxs, !allSwaps) = pairValues tx1InfosA tx2Infos
      
      -- Filter out liquidity operations
      !relevantSwaps = removeLiqTokens mintAssets allSwaps
      
      !finalExchanges = map (classifySwap tokenClass) relevantSwaps

      -- adaReserves = 0 -- TEMP

  return (finalExchanges, tokenLiq, tokenClass, adaReserves, poolAsset, length unmatchedTxs, map fst tx1InfosR)

-- Note that the second value in the output is
-- the liquidity in the dex after the swap.

-- | Convert an `IndividualSwap` into an `Exchange`.
-- Might need to add finer classification as time
-- goes on.
classifySwap :: AssetClass -> IndividualSwap -> (FullTransaction, Exchange, BS.ByteString)
classifySwap ac isw
  | assetLookup ac (iswAssets1 isw) == 0
  = (iswTx isw, ExBuy  ac (assetLookup ac (iswAssets2 isw)) (iswAda1 isw - 4_500_000), iswAddress isw) -- - iswAda2 isw))
  | assetLookup ac (iswAssets2 isw) == 0
  = (iswTx isw, ExSell ac (assetLookup ac (iswAssets1 isw)) (iswAda2 isw - 2_000_000), iswAddress isw) -- - iswAda1 isw))
  | otherwise -- iswAda1 isw == 4_500_000
  = (iswTx isw, ExSell ac (assetLookup ac (iswAssets1 isw)) (iswAda2 isw - 2_000_000), iswAddress isw) -- - iswAda1 isw))
  -- | otherwise
  -- = (iswTxHash isw, ExBuy  ac (assetLookup ac (iswAssets2 isw)) (iswAda1 isw)) -- - iswAda2 isw))

-- | Convert a pair of `Maybe`s into a `Maybe` of a pair.
maybePair :: forall (a :: Type) (b :: Type). (Maybe a, Maybe b) -> Maybe (a,b)
maybePair (Nothing, _) = Nothing
maybePair (_, Nothing) = Nothing
maybePair (Just x, Just y) = Just (x,y)

-- | A simple pair to help calculation
data ValuePair = ValuePair
  { vpAddress :: BS.ByteString
  , vpAda     :: Integer
  , vpAssets  :: AssetMap
  -- , vpTime    :: Maybe POSIXTime
  } deriving (Show,Eq)

txOutToVP :: TxOutput -> ValuePair
txOutToVP txo =
  ValuePair
    (txoAddress txo)
    (txoAmount  txo)
    (fromAssetList $ txoAssets txo)

-- | Pair up two lists of values.
pairValues :: [(FullTransaction,ValuePair)] -> [ValuePair] -> ([ValuePair], [IndividualSwap])
pairValues vps1 vps2 =
  mapEither (\vp -> pairX vp <$> findX vp vps2) vps1
  where
    findX (_,vp) = findOrElse vp (\vp' -> vpAddress vp == vpAddress vp')
    pairX (tx,vp1) vp2 =
      IndividualSwap
        (vpAddress vp1)
        tx -- (txtHash $ ftTransaction tx)
        (vpAda vp1)
        (vpAda vp2)
        (vpAssets vp1)
        (vpAssets vp2)

-- | Partition list into pairs that are from the same
-- wallet, and others.
findCopies :: [(a, ValuePair)] -> ([(a, ValuePair)],[(a, ValuePair)])
findCopies = findCopies' [] []

findCopies' :: [(a, ValuePair)] -> [(a, ValuePair)] -> [(a, ValuePair)] -> ([(a, ValuePair)],[(a, ValuePair)])
findCopies' dups norms [] = (dups,norms)
findCopies' dups norms (elmt@(txHsh, vp) : rst)
  | ([],othrs)     <- rslts
  = findCopies' dups (elmt:norms) othrs
  | (copies,othrs) <- rslts
  = findCopies' (elmt:(copies ++ dups)) norms othrs
  where
    wallet = vpAddress vp
    rslts = partition (\(_,v) -> vpAddress v == wallet) rst

-- | Remove any Liquidity tokens from the swaps.
-- (Could probably make more efficient)
removeLiqTokens :: [AssetClass] -> [IndividualSwap] -> [IndividualSwap]
removeLiqTokens [] xs = xs
removeLiqTokens (ast:asts) xs
  = removeLiqTokens asts $ filter checkSwap xs
  where
    checkSwap :: IndividualSwap -> Bool
    checkSwap isw =
      lookupAsset (iswAssets1 isw) ast == 0
        && lookupAsset (iswAssets2 isw) ast == 0

-- lookupAsset :: AssetMap -> AssetClass -> Integer
-- assetLookup :: AssetClass -> AssetMap -> Integer

-- | A type for a full, matched swap.
data IndividualSwap = IndividualSwap
  { iswAddress :: BS.ByteString
  , iswTx   :: FullTransaction
  , iswAda1 :: Integer 
  , iswAda2 :: Integer
  , iswAssets1 :: AssetMap
  , iswAssets2 :: AssetMap
  } deriving (Show, Eq)

-- | Like `find`, but with a backup response.
findOrElse :: forall (a :: Type) (b :: Type). b -> (a -> Bool) -> [a] -> Either b a
findOrElse x p xs = case find p xs of
  Nothing -> Left  x
  Just y  -> Right y

mapEither :: (a -> Either b c) -> [a] -> ([b],[c])
mapEither f xs = partitionEithers $! map f xs

-- | Guess the address of the wallet.
guessWallet :: ExtraTransaction -> Maybe BS.ByteString
guessWallet etx = 
  fmap fst $ uncons $ filter checkRelevantAddr $ fmap (txoAddress . cmoTxOut) $ ftOutputs $ etxTrans etx

-- | Guess the address of the wallet, using the 
-- outputs of the second transaction.
guessWallet' :: [BS.ByteString] -> ExtraTransaction -> Maybe BS.ByteString
guessWallet' adrs etx = 
  fmap fst $ uncons $ filterRelevantAddrs adrs $ fmap (txoAddress . cmoTxOut) $ ftOutputs $ etxTrans etx


-- | Get the actual value deposited into SundaeSwap.
getEscrow :: BS.ByteString -> ExtraTransaction -> Maybe (Integer, AssetMap)
getEscrow sunAddr etx = (txoAmount &&& fromAssetList . txoAssets) <$> txo
  -- (0, AssetMap Map.empty)
  where
    outpIx = etxId etx
    outpts = ftOutputs $ etxTrans etx
    theOut = find (\cmo -> {-cmoTxId cmo == outpIx &&-} compOutput cmo == sunAddr) outpts
    txo = cmoTxOut <$> theOut

-- | Get the output address of a `CmOutput`.
compOutput :: CmOutput -> BS.ByteString
compOutput = txoAddress . cmoTxOut

-- | Try and calculate the value of
-- a trade that was made. 
calculatePrice :: BS.ByteString -> SwapPair -> Maybe (Exchange, Integer) -- TempSale -- TokenSale
calculatePrice sunAddr sp = do 
  let tx1utxos :: NonEmpty.NonEmpty [CmOutput]
      tx1utxos = ftOutputs . etxTrans <$> swapPart1 sp
      tx1id :: NonEmpty.NonEmpty Integer
      tx1id    = etxId <$> swapPart1 sp
      tx1usrs  = NonEmpty.map (filter checkRelevantAddr . map (txoAddress . cmoTxOut)) tx1utxos
      tx1Outp' = NonEmpty.zipWith (\xot xid -> find (\x -> cmoTxId x == xid) xot) tx1utxos tx1id
      tx1usrs' = concat $ NonEmpty.toList tx1usrs

  -- The TxOutput from part1 we are interested in.
  -- tx1Outp <- traverse (find (\x -> cmoTxId x == tx1id)) tx1utxos
  tx1Outp <- sequenceA tx1Outp'
  
  let tx2utxos = ftOutputs $ swapPart2 sp
      -- the amount going out to the user.
      tx2usrs  = filter (checkRelevantAddr . txoAddress . cmoTxOut) tx2utxos
      tx2usrs' = filter ((`elem` tx1usrs') . txoAddress . cmoTxOut) tx2usrs -- found in utxo1 user list.
      -- the amount going to the user(s)
      tx2sun  = filter ((== sunAddr) . txoAddress . cmoTxOut) tx2utxos
      tx2sunM = foldMap (fromAssetList . txoAssets . cmoTxOut) tx2sun
      tx2map  = foldMap (fromAssetList . txoAssets . cmoTxOut) tx2usrs
      tx2map' = foldMap (fromAssetList . txoAssets . cmoTxOut) tx2usrs'
      -- Ada going out to the users.
      tx2ada  = sum $ map (txoAmount . cmoTxOut) tx2usrs
      !tx2ada' = sum $ map (txoAmount . cmoTxOut) tx2usrs'
      -- The amount going to Sundae.


      -- txoAmount (cmoTxOut tx1Outp) == 4.5 Ada
      -- seems like it's a fee
  
  -- Getting the swap assets
  tx2sun'   <- cmoTxOut <$> maybeSingleton tx2sun
  let tx2sunAsts = txoAssets tx2sun'
  tx2swapAsset <- find (\ast -> astAmount ast == 1) tx2sunAsts
  tx2soldAsset <- find (\ast -> astAmount ast >  1) tx2sunAsts
  
  -- tx2soldAsset is the one we should be looking at.
  let tx2soldAc = getAssetClass tx2soldAsset
      soldAmt   = lookupAsset tx2map' tx2soldAc
      tx1Assets = sconcat $ txoAssets . cmoTxOut <$> tx1Outp
      tx1tokens = assetLookup tx2soldAc $ fromAssetList tx1Assets
      tx1Ada    = sum $ txoAmount . cmoTxOut <$> tx1Outp
      !sunTokens = assetLookup tx2soldAc tx2sunM
  
  if soldAmt == 0
     -- The user is selling tokens
     then return (ExSell tx2soldAc tx1tokens tx2ada', sunTokens)
     -- The user is buying tokens
     else return (ExBuy  tx2soldAc soldAmt tx1Ada , sunTokens)

  -- return $ TokenSale (TxAsset 0 "" "") 0 -- temp
  -- return (tx2soldAc, soldAmt, txoAmount $ cmoTxOut tx1Outp) -- temp

-- | Check if the address is one we want to
-- consider; i.e. it's a user wallet. 
-- Hopefully my temp check works.
-- Deprecated; use `filterRelevantAddrs`
-- instead.
checkRelevantAddr :: BS.ByteString -> Bool
checkRelevantAddr bs = BS.length bs > 60

-- | Like `checkRelevantAddr`, this checks that
-- the wallet is one we care about. i.e. that
-- it's one of the ones in the list of part2
-- addresses, and that it's long enough.
filterRelevantAddrs :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
filterRelevantAddrs adrs bss -- = BS.length bs > 60
  | null filtBss1
  = filter (\bs -> BS.length bs > 60) bss
  | null filtBss2
  = filtBss1
  | otherwise = filtBss2
  where 
    filtBss1 = filter (`elem` adrs) bss
    filtBss2 = filter (\bs -> BS.length bs > 60) filtBss1

maybeSingleton :: forall (a :: Type). [a] -> Maybe a
maybeSingleton [x] = Just x
maybeSingleton  _  = Nothing

-- | Get the Block number.
getBlockNumber :: SwapPair -> Integer
getBlockNumber (SwapPair _ (FullTransaction {ftBlockNumber = bnum}) _) = bnum

-- | Get the token that indicates the pair
-- of tokens.
getLiquidityToken :: BS.ByteString -> SwapPair -> Maybe AssetClass
getLiquidityToken adr SwapPair {swapPart2 = ftx} =
  let !txOuts = map cmoTxOut $ ftOutputs ftx
      !relTxo = filter (\txo -> txoAddress txo == adr) txOuts
      !tokens = concatMap txoAssets relTxo
      !solos  = filter (\ast -> astAmount ast == 1) tokens
      -- policy is 0029cb7c88c7567b63d1a512c0ed626aa169688ec980730c0473b913
  in getAssetClass <$> maybeSingleton solos

-- Possible way to filter out unused tokens:
-- See what address they came from (will
-- likely require a chain explorer) and then
-- see how much goes back to the same address.

-- One input from ...znu has all the 
-- tokens of the correct asset.

-- One input from ...tqh8 that was 
-- output in the first transaction.

-- https://cardanoscan.io/transaction/70d20100e60734523ef83aa33cf944c3a7dbf7c37ceabbd1342a0f243afe9424
-- https://cardanoscan.io/transaction/d566f854e95a0f3fa5377aa686077f44ff9ec7211d61029b0acc6aa0d0d7e232

-- common outputs from part2:
-- addr1w9jx45flh83z6wuqypyash54mszwmdj8r64fydafxtfc6jgrw4rm3
-- addr1vxalsdnqeatddcn8mwtzuqe0sznf3ctk5pwcemy440sapasgpxx05


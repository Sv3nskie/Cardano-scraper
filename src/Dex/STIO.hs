{-# LANGUAGE RankNTypes #-}

module Dex.STIO (
  STHandle,
  withSTHandle,
  withSTHandle_,
  withSTHandleP,
  withSTHandleP_,
  sPutStr,
  sPutStrT,
  sPutStrBS,
  sPutStrLn,
  sPutStrLnT,
  sPutStrLnBS,
  sPrint,
) where

import Control.Monad.ST.Strict
import Data.ByteString qualified as BS
import Data.Kind 
import Data.STRef
import Data.Text (Text)
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

-- May want to rewrite to use
-- Data.Text.Lazy.Builder.

-- | A handle for a single outputter.
newtype STHandle s = STHandle (STRef s Text)

-- | Run an action that perfroms output to some `Text`.
withSTHandle :: (forall s. STHandle s -> ST s a) -> (a, Text) -- (STHandle s -> ST s a) -> ST s (a, Text)
withSTHandle fa = runST $ do
  hnd <- newSTRef T.empty
  rsl <- fa (STHandle hnd)
  txt <- readSTRef hnd
  return (rsl,txt)

-- | Like `withSTHandle`, but ignores the result of the action.
withSTHandle_ :: (forall s. STHandle s -> ST s ()) -> Text -- (STHandle s -> ST s a) -> ST s (a, Text)
withSTHandle_ fa = runST $ do
  hnd <- newSTRef T.empty
  _   <- fa (STHandle hnd)
  readSTRef hnd

-- | Like `withSTHandle`, but with a prior Text value.
withSTHandleP :: Text -> (forall s. STHandle s -> ST s a) -> (a, Text) -- (STHandle s -> ST s a) -> ST s (a, Text)
withSTHandleP tx fa = runST $ do
  hnd <- newSTRef tx
  rsl <- fa (STHandle hnd)
  txt <- readSTRef hnd
  return (rsl,txt)

-- | Like `withSTHandleP`, but ignores the result of the action.
withSTHandleP_ :: Text -> (forall s. STHandle s -> ST s ()) -> Text -- (STHandle s -> ST s a) -> ST s (a, Text)
withSTHandleP_ tx fa = runST $ do
  hnd <- newSTRef tx
  _   <- fa (STHandle hnd)
  readSTRef hnd

-- | Write a `String` to the `STHandle`.
sPutStr :: forall s. STHandle s -> String -> ST s ()
sPutStr (STHandle strf) str = do
  modifySTRef' strf (<> T.pack str)

-- | Write some `Text` to the `STHandle`.
sPutStrT :: forall s. STHandle s -> Text -> ST s ()
sPutStrT (STHandle strf) txt = do
  modifySTRef' strf (<> txt)

-- | Write a `BS.ByteString` to the `STHandle`,
-- interpreted as a UTF-8 encoded string.
sPutStrBS :: forall s. STHandle s -> BS.ByteString -> ST s ()
sPutStrBS (STHandle strf) bs = do
  modifySTRef' strf (<> T.decodeUtf8 bs)

-- | Write a `String` to the `STHandle`, 
-- followed by a newline.
sPutStrLn :: forall s. STHandle s -> String -> ST s ()
sPutStrLn (STHandle strf) str = do
  modifySTRef' strf (<> T.pack str <> "\n")

-- | Write some `Text` to the `STHandle`, 
-- followed by a newline.
sPutStrLnT :: forall s. STHandle s -> Text -> ST s ()
sPutStrLnT (STHandle strf) txt = do
  modifySTRef' strf (<> txt <> "\n")

-- | Write a `BS.ByteString` to the `STHandle`,
-- interpreted as a UTF-8 encoded string,
-- followed by a newline.
sPutStrLnBS :: forall s. STHandle s -> BS.ByteString -> ST s ()
sPutStrLnBS (STHandle strf) bs = do
  modifySTRef' strf (<> T.decodeUtf8 bs <> "\n")

-- | Like `print`, this function takes
-- a `Show`able value and prints it
-- to the `STHandle`, followed by
-- a newline.
sPrint :: forall (a :: Type) s. (Show a) => STHandle s -> a -> ST s ()
sPrint (STHandle strf) x = do
  modifySTRef' strf (<> T.pack (show x) <> "\n")

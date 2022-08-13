module Dex.IO (
  -- * Sinks and Output
  ConSink,
  withConSink,
  withConSinkH,
  directFlush,
  directFlushT,
  -- * Handles
  ConHandle,
  newConHandle,
  newConHandleIO,
  cFlush,
  -- * Output
  cPutStr,
  cPutStrT,
  cPutStrBS,
  cPutStrLn,
  cPutStrLnT,
  cPutStrLnBS,
  cPrint,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.IORef
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString    qualified as BS
import System.IO
import Control.Monad (forever)

-- | A Sink to create 
data ConSink = ConSink Handle (TQueue Text)

-- | A handle for a single outputter.
data ConHandle = ConHandle (TVar Text) ConSink

-- | Start a `ConSink` where anything
-- flushed to it from a `ConHandle` will
-- be synchronised.
withConSink :: (ConSink -> IO a) -> IO a
withConSink = withConSinkH stdout

-- | Like `withConSink`, but to
-- any user-speicified `Handle`.
withConSinkH :: Handle -> (ConSink -> IO a) -> IO a
withConSinkH hnd f = do
  cs <- ConSink hnd <$> newTQueueIO
  withAsync (queueWriter cs) (\_ -> f cs)

-- (Not Exported)
-- continuously print text from
-- a specific `ConSink`.
queueWriter :: ConSink -> IO ()
queueWriter (ConSink hnd tq) = forever $ do
  txt <- atomically $ readTQueue tq
  T.hPutStr hnd txt

-- | Create a new `ConHandle` in an `STM` transaction.
newConHandle :: ConSink -> STM ConHandle
newConHandle cs = ConHandle <$> newTVar "" <*> pure cs

-- | Create a new `ConHandle` in the `IO` monad.
newConHandleIO :: ConSink -> IO ConHandle
newConHandleIO cs = ConHandle <$> newTVarIO "" <*> pure cs

-- | Write a `String` to the `ConHandle`.
cPutStr :: ConHandle -> String -> STM ()
cPutStr (ConHandle tv _tq) str = do
  modifyTVar' tv (<> T.pack str)

-- | Write some `Text` to the `ConHandle`.
cPutStrT :: ConHandle -> Text -> STM ()
cPutStrT (ConHandle tv _tq) txt = do
  modifyTVar' tv (<> txt)

-- | Write a `BS.ByteString` to the `ConHandle`,
-- interpreted as a UTF-8 encoded string.
cPutStrBS :: ConHandle -> BS.ByteString -> STM ()
cPutStrBS (ConHandle tv _tq) bs = do
  modifyTVar' tv (<> T.decodeUtf8 bs)

-- | Write a `String` to the `ConHandle`, 
-- followed by a newline.
cPutStrLn :: ConHandle -> String -> STM ()
cPutStrLn (ConHandle tv _tq) str = do
  modifyTVar' tv (<> T.pack str <> "\n")

-- | Write some `Text` to the `ConHandle`, 
-- followed by a newline.
cPutStrLnT :: ConHandle -> Text -> STM ()
cPutStrLnT (ConHandle tv _tq) txt = do
  modifyTVar' tv (<> txt <> "\n")

-- | Write a `BS.ByteString` to the `ConHandle`,
-- interpreted as a UTF-8 encoded string,
-- followed by a newline.
cPutStrLnBS :: ConHandle -> BS.ByteString -> STM ()
cPutStrLnBS (ConHandle tv _tq) bs = do
  modifyTVar' tv (<> T.decodeUtf8 bs <> "\n")

-- | Like `print`, this function takes
-- a `Show`able value and prints it
-- to the `ConHandle`, followed by
-- a newline.
cPrint :: forall (a :: Type). (Show a) => ConHandle -> a -> STM ()
cPrint (ConHandle tv _cs) x = do
  modifyTVar' tv (<> T.pack (show x) <> "\n")

-- | Flush the `Text` to the Queue,
-- where it will be written by a 
-- thread connected to the Queue.
cFlush :: ConHandle -> STM ()
cFlush (ConHandle tv (ConSink _hnd tq)) = do
  txt <- readTVar tv
  writeTVar tv ""
  writeTQueue tq txt

-- | Print a `String` directly to
-- the Queue. Useful when you aren't
-- building up a large, multi-part
-- string.
directFlush :: ConSink -> String -> STM ()
directFlush (ConSink _hnd tq) str =
  writeTQueue tq (T.pack str <> "\n")

-- | Print some `Text` directly to
-- the Queue. Useful when you aren't
-- building up a large, multi-part
-- string.
directFlushT :: ConSink -> Text -> STM ()
directFlushT (ConSink _hnd tq) txt =
  writeTQueue tq (txt <> "\n")


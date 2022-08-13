module Dex.Database.Initialise (
  performDB,
  performDB',
  runDB,
  authenticate,
) where

import Control.Monad.STM (atomically)
import Control.Exception (bracket)
import Control.Monad (unless, void)
import Database.MongoDB
import Dex.Arguments
import Dex.Database.Types.Setup
-- import System.IO (hPutStrLn, stderr)
import Dex.IO
import Control.Concurrent.QSem (QSem)

-- | Bracket-like support for MongoDB
-- database actions.
performDB :: Host -> (Pipe -> IO c) -> IO c
performDB hst =
  bracket (connect hst) close

-- | More specialised version of `performDB`.
performDB' :: Host -> Database -> Action IO c -> IO c
performDB' hst db act =
  bracket
    (connect hst)
    close
    (\pipe -> access pipe master db act)

runDB :: ConSink -> Host -> Database -> Maybe UserAccess -> QSem -> (Maybe DBSetup -> IO c) -> IO c
runDB errSink hst db Nothing qsem = ($ Nothing) -- equiv to runDB _ _ Nothing f = f Nothing
runDB errSink hst db (Just uacs) qsem =
  bracket 
    ( do
        pip <- connect hst
        bl  <- access pip master db $ authenticate uacs 
        if bl
          then do
            atomically $ directFlushT errSink "Successfully authenticated."
            return (Just $ DBSetup pip master db qsem)
          else do
            -- close pip
            atomically $ directFlushT errSink "Could not authenticate DB connection."
            -- return (Just $ DBSetup pip master db tsem)
            return Nothing
            -- return Nothing
    )
    ( \case
        Nothing    -> return ()
        (Just (DBSetup pip _ _ _)) -> void $ close pip
    )
    

authenticate :: UserAccess -> Action IO Bool
authenticate (UserAccess usr pwd) = useDb "admin" (auth usr pwd)

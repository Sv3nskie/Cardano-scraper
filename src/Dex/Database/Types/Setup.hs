module Dex.Database.Types.Setup (
  DBSetup(..),
  DBException(..),
  accessAct,
  accessMaybe,
  accessCatch,
) where

-- import Control.Monad.STM (atomically)
import Control.Concurrent.QSem
import Control.Exception (Exception)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.MongoDB (AccessMode, Pipe, Database, access, Action, Failure)


-- | The various aspects of the 
data DBSetup = DBSetup 
  { dbsPipe   :: Pipe
  , dbsAccess :: AccessMode
  , dbsDB     :: Database 
  , dbSem     :: QSem
  } 

accessAct' :: MonadIO m => DBSetup -> Action m a -> m a
accessAct' (DBSetup pip acs db qsem) = access pip acs db

accessAct :: (MonadIO m, MonadMask m) => DBSetup -> Action m a -> m a
accessAct (DBSetup pip acs db qsem) act = 
  bracket_
    (liftIO $ waitQSem   qsem)
    (liftIO $ signalQSem qsem)
    (access pip acs db act)

accessMaybe :: (MonadIO m, MonadMask m) => Maybe DBSetup -> Action m a -> m (Maybe a)
accessMaybe Nothing    _   = return Nothing
accessMaybe (Just dbs) act = Just <$> accessAct dbs act

accessCatch :: (MonadIO m, MonadCatch m, MonadMask m) => Maybe DBSetup -> Action m a -> m (Either DBException a)
accessCatch Nothing _ = return (Left EmptySetup)
accessCatch (Just dbs) act =
  catch (Right <$> accessAct dbs act) (return . Left . DBError)

data DBException
  = EmptySetup
  | DBError Failure
  deriving stock (Show, Eq)

instance Exception DBException



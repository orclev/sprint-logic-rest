module Types where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Resource (MonadResource (..), runResourceT)
import Database.Persist.Sqlite
import Database.Persist.Sql
import Rest
import Rest.Info
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger

data ServerData = ServerData { dbPool :: ConnectionPool }

newtype SiteApi a = SiteApi { unSiteApi :: ReaderT ServerData IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           )

runSiteApi :: ServerData -> SiteApi a -> IO a
runSiteApi serverData = flip runReaderT serverData . unSiteApi

runDB :: (MonadIO m, MonadReader ServerData m) => SqlPersistM o -> m o
runDB action = do
  pool <- asks dbPool
  liftIO . runResourceT . runNoLoggingT $ runSqlPool action pool
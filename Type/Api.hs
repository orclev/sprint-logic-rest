module Type.Api 
  ( ServerData (..)
  , SiteApi (..)
  , module E
  , runSiteApi
  , runDB
  )where

import Control.Applicative (Applicative)
import Control.Monad.Reader as E (MonadReader, ReaderT (..), asks)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Resource (MonadResource (..), runResourceT)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPool)
import Control.Monad.IO.Class as E (liftIO)
import Control.Monad.Logger (runNoLoggingT)

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
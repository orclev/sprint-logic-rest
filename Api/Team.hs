module Api.Team where

import Type.Core
import Type.Team
import Type.Api
import Type.CreateTeam
import Control.Monad.Reader (ReaderT (..), asks)
import Rest (jsonO,someO,jsonI,someI,jsonE,someE,Resource,Void,mkResourceReader
            ,withListing,unnamedSingleRead,Handler,mkIdHandler,Reason (..),mkListing
            ,mkInputHandler,ListHandler,domainReason)
import Rest.Info (Info (describe))
import Rest.Types.ShowUrl (ShowUrl (showUrl))
import qualified Rest.Resource as R
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Error (ErrorT, throwError)
import Control.Applicative ((<$>),(<*>))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB

type WithTeam = ReaderT ResourceIdent SiteApi

instance Info ResourceIdent where
  describe _ = "identifier"

instance ShowUrl ResourceIdent where
  showUrl = show

resource :: Resource SiteApi WithTeam ResourceIdent () Void
resource = mkResourceReader
  { R.name = "team"
  , R.schema = withListing () $ unnamedSingleRead id
  , R.get = Just get
  , R.create = Just create
  , R.list = const list
  }

stubTeam :: Team
stubTeam = Team (RID TeamR "yoIrTg9GtlkKEPuKb8BbOF") "Stub"

get :: Handler WithTeam
get = mkIdHandler (jsonE . someE . jsonO . someO) findUser
  where
    findUser :: () -> ResourceIdent -> ErrorT (Reason TeamError) WithTeam Team
    findUser _ rid = do
      mTeam <- (lift . lift) (getTeam rid)
      maybe (throwError NotFound) (return) mTeam

getTeam :: ResourceIdent -> SiteApi (Maybe Team)
getTeam rid = do
  mEntity <- runDB $ (DB.getBy $ UniqueIdent rid)
  return $ DB.entityVal <$> mEntity
    
list :: ListHandler SiteApi
list = mkListing (jsonO . someO) $ \r -> runDB teamList

teamList :: DB.SqlPersistM [Team]
teamList = do
  teams <- DB.selectList [] []
  return $ map DB.entityVal teams

create :: Handler SiteApi
create = mkInputHandler (jsonI . someI . jsonE . someE . jsonO . someO) insertTeam

insertTeam :: CreateTeam -> ErrorT (Reason TeamError) SiteApi Team
insertTeam (CreateTeam n) = do
  rid <- liftIO $ randomRid TeamR
  let newTeam = Team rid n
  mVal <- runDB $ insertTeamDB' newTeam
  maybe (throwError $ domainReason (const 400) UnknownError) return mVal

insertTeamDB' :: Team -> DB.SqlPersistM (Maybe Team)
insertTeamDB' x = do
  key <- DB.insert x
  DB.get key
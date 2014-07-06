module Api.Team 
  ( resource
  , WithTeam
  , TeamIdent
  ) where

import Type.Core
import Type.Team
import Type.Api
import Type.CreateTeam
import Rest (jsonO,someO,jsonI,someI,jsonE,someE,Resource,Void,mkResourceReader
            ,withListing,unnamedSingleRead,Handler,mkIdHandler,Reason (..),mkListing
            ,mkInputHandler,ListHandler,domainReason)
import qualified Rest.Resource as R
import Control.Applicative ((<$>),(<*>))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB

type WithTeam = ReaderT TeamIdent SiteApi

resource :: Resource SiteApi WithTeam TeamIdent () Void
resource = mkResourceReader
  { R.name = "team"
  , R.schema = withListing () $ unnamedSingleRead id
  , R.get = Just get
  , R.create = Just create
  , R.list = const list
  }

get :: Handler WithTeam
get = mkIdHandler (jsonE . someE . jsonO . someO) findTeam
  where
    findTeam :: () -> TeamIdent -> ErrorT (Reason TeamError) WithTeam Team
    findTeam _ rid = do
      mTeam <- (lift . lift) (getTeam rid)
      maybe (throwError NotFound) (return) mTeam

getTeam :: TeamIdent -> SiteApi (Maybe Team)
getTeam rid = do
  mEntity <- runDB $ (DB.getBy $ UniqueTeamIdent rid)
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
insertTeam newTeam = do
  nt <- createFrom newTeam
  mVal <- runDB $ do
    key <- DB.insert nt
    DB.get key
  maybe (throwError $ domainReason (const 400) UnknownError) return mVal
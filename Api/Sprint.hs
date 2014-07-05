module Api.Sprint 
  ( resource
  , WithSprint
  ) where

import Type.Core
import Type.Sprint
import qualified Type.CreateSprint as C
import Type.Api
import Api.Team (WithTeam)
import Rest (jsonO,someO,jsonI,someI,jsonE,someE,Resource,Void,mkResourceReader
            ,withListing,unnamedSingleRead,Handler,mkIdHandler,Reason (..),mkListing
            ,mkInputHandler,ListHandler,domainReason)
import qualified Rest.Resource as R
import Control.Applicative ((<$>),(<*>))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Esqueleto as S hiding (get)

type WithSprint = ReaderT ResourceIdent WithTeam

resource :: Resource WithTeam WithSprint ResourceIdent () Void
resource = mkResourceReader
  { R.name = "sprint"
  , R.schema = withListing () $ unnamedSingleRead id
  , R.get = Just get
  , R.create = Just create
  , R.update = Just updateS
  , R.list = const list
  }

get :: Handler WithSprint
get = mkIdHandler (jsonE . someE . jsonO . someO) findSprint
  where
    findSprint :: () -> ResourceIdent -> ErrorT (Reason SprintError) WithSprint Sprint
    findSprint _ rid = do
      mSprint <- (lift . lift . lift) (getSprint rid)
      maybe (throwError NotFound) (return) mSprint

getSprint :: ResourceIdent -> SiteApi (Maybe Sprint)
getSprint rid = do
  mEntity <- runDB $ (DB.getBy $ UniqueSprintIdent rid)
  return $ DB.entityVal <$> mEntity
    
list :: ListHandler WithTeam
list = mkListing (jsonO . someO) $ \r -> (lift . lift) . runDB $ sprintList

sprintList :: DB.SqlPersistM [Sprint]
sprintList = do
  sprints <- DB.selectList [] []
  return $ map DB.entityVal sprints

create :: Handler WithTeam
create = mkInputHandler (jsonI . someI . jsonE . someE . jsonO . someO) insertSprint

insertSprint :: C.CreateSprint -> ErrorT (Reason SprintError) WithTeam Sprint
insertSprint newSprint = do
  ns <- createFrom newSprint
  mVal <- (lift . lift) $ runDB $ do
    key <- DB.insert ns
    DB.get key
  maybe (throwError $ domainReason (const 400) UnknownError) return mVal

updateS :: Handler WithSprint
updateS = mkIdHandler (jsonI . someI . jsonE . someE . jsonO . someO) updateSprint

updateSprint :: C.CreateSprint -> ResourceIdent -> ErrorT (Reason SprintError) WithSprint Sprint
updateSprint ns sprintId = do
  mVal <- (lift . lift . lift) $ runDB $ do
    S.update $ \s -> do
      set s [ SprintNumber =. val (C.sprintNumber ns)
            , SprintPeople =. val (C.sprintPeople ns)
            , SprintWorkDays =. val (C.sprintWorkDays ns)
            , SprintVacationDays =. val (C.sprintVacationDays ns)
            , SprintInterruptHours =. val (C.sprintInterruptHours ns)
            , SprintPlannedPoints =. val (C.sprintPlannedPoints ns)
            , SprintDeliveredPoints =. val (C.sprintDeliveredPoints ns)
            ]
      where_ (s ^. SprintIdent ==. val sprintId)
    DB.getBy $ UniqueSprintIdent sprintId
  maybe (throwError $ domainReason (const 400) UnknownError) (return . DB.entityVal) mVal

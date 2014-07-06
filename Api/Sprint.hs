module Api.Sprint 
  ( resource
  , WithSprint
  , SprintIdent
  ) where

import Type.Core
import Type.Sprint
import qualified Type.CreateSprint as C
import Type.Api
import Api.Team (WithTeam, TeamIdent)
import Rest
import qualified Rest.Resource as R
import Control.Applicative ((<$>),(<*>))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Esqueleto as S hiding (get)

data SprintId = ById SprintIdent | ByNumber Int | Latest

type WithSprint = ReaderT SprintId WithTeam

resource :: Resource WithTeam WithSprint SprintId () Void
resource = mkResourceReader
  { R.name = "sprint"
  , R.schema = withListing () $ named [("id", singleRead ById)
                                      ,("number", singleRead ByNumber)
                                      ,("latest", single Latest)
                                      ]
  , R.get = Just get
  , R.create = Just create
  , R.update = Just updateS
  , R.remove = Just remove
  , R.list = const list
  }

get :: Handler WithSprint
get = mkIdHandler (jsonE . someE . jsonO . someO) findSprint
  where
    findSprint :: () -> SprintId -> ErrorT (Reason SprintError) WithSprint Sprint
    findSprint _ sprintId = do
      rid <- lift . lift $ getUniqueSelector sprintId
      mSprint <- lift . lift . lift $ getSprint rid
      maybe (throwError NotFound) (return) mSprint

getUniqueSelector :: SprintId -> WithTeam (Unique (SprintGeneric SqlBackend))
getUniqueSelector (ById rid) = return $ UniqueSprintIdent rid
getUniqueSelector Latest = do
  teamId <- ask
  return $ UniqueSprintLatest teamId Active
getUniqueSelector (ByNumber n) = do
  teamId <- ask
  return $ UniqueSprintNumber teamId n

getLatest :: TeamIdent -> SqlPersistM (Maybe Sprint)
getLatest teamId = do
  mSprint <- DB.getBy $ UniqueSprintLatest teamId Active
  return $ DB.entityVal <$> mSprint

getSprint :: Unique (SprintGeneric SqlBackend) -> SiteApi (Maybe Sprint)
getSprint rid = do
  mEntity <- runDB $ (DB.getBy $ rid)
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
  ns <- lift $ makeSprint newSprint
  teamId <- lift ask
  mVal <- (lift . lift) $ runDB $ do
    mLatest <- getLatest teamId
    let n = maybe 1 ((+1) . sprintNumber) mLatest
    S.update $ \s -> do
      set s [ SprintLatest =. val Inactive ]
      where_ (s ^. SprintLatest ==. val Active &&. s ^. SprintTeam ==. val teamId)
    key <- DB.insertUnique $ ns { sprintNumber = n, sprintLatest = Active }
    maybe (return Nothing) DB.get key
  maybe (throwError $ domainReason (const 409) SprintExists) return mVal

makeSprint :: C.CreateSprint -> WithTeam (SprintGeneric SqlBackend)
makeSprint = createFrom

remove :: Handler WithSprint
remove = mkIdHandler id deleteSprint

deleteSprint :: () -> SprintId -> ErrorT (Reason ()) WithSprint ()
deleteSprint _ sprintId = do
  teamId <- lift . lift $ ask
  sel <- lift . lift $ getUniqueSelector sprintId
  lift . lift . lift $ runDB $ do
    DB.deleteBy sel

updateS :: Handler WithSprint
updateS = mkIdHandler (jsonI . someI . jsonE . someE . jsonO . someO) updateSprint

updateSprint :: C.CreateSprint -> SprintId -> ErrorT (Reason SprintError) WithSprint Sprint
updateSprint ns rid = do
  teamId <- lift . lift $ ask
  sel <- lift . lift $ getUniqueSelector rid
  mVal <- (lift . lift . lift) $ runDB $ do
    S.update $ \s -> do
      set s [ SprintPeople =. val (C.sprintPeople ns)
            , SprintWorkDays =. val (C.sprintWorkDays ns)
            , SprintVacationDays =. val (C.sprintVacationDays ns)
            , SprintInterruptHours =. val (C.sprintInterruptHours ns)
            , SprintPlannedPoints =. val (C.sprintPlannedPoints ns)
            , SprintDeliveredPoints =. val (C.sprintDeliveredPoints ns)
            ]
      (getWhereClause s rid teamId)
    DB.getBy sel
  maybe (throwError $ domainReason (const 400) UnknownError) (return . DB.entityVal) mVal
  where
    getWhereClause s (ById sprintId) _ = where_ (s ^. SprintIdent ==. val sprintId)
    getWhereClause s (ByNumber n) teamId = where_ (s ^. SprintNumber ==. val n &&. s ^. SprintTeam ==. val teamId)
    getWhereClause s Latest teamId = where_ (s ^. SprintLatest ==. val Active &&. s ^. SprintTeam ==. val teamId)
module Type.CreateSprint 
  ( CreateSprint (..)
  ) where

import Type.Core
import Control.Monad.Reader (MonadReader, ask)
import qualified Type.Sprint as S (Sprint (..), SprintGeneric (..))
import Database.Persist.TH (derivePersistField)

data CreateSprint = CreateSprint 
  { sprintNumber :: Int
  , sprintPeople :: Int 
  , sprintWorkDays :: Int
  , sprintVacationDays :: Int
  , sprintInterruptHours :: Maybe Int
  , sprintPlannedPoints :: Int
  , sprintDeliveredPoints :: Maybe Int
  } deriving (Eq, Generic, Show, Typeable)

instance JSONSchema CreateSprint where schema = gSchema
instance ToJSON CreateSprint
instance FromJSON CreateSprint

instance (MonadIO m, MonadReader ResourceIdent m) => CreateAble m CreateSprint S.Sprint where
  createFrom sprint = do
    teamId <- ask
    rid <- liftIO $ randomRid SprintR 
    return $ S.Sprint
      { S.sprintTeam = teamId
      , S.sprintIdent = rid
      , S.sprintNumber = sprintNumber sprint
      , S.sprintPeople = sprintPeople sprint
      , S.sprintWorkDays = sprintWorkDays sprint
      , S.sprintVacationDays = sprintVacationDays sprint
      , S.sprintInterruptHours = sprintInterruptHours sprint
      , S.sprintPlannedPoints = sprintPlannedPoints sprint
      , S.sprintDeliveredPoints = sprintDeliveredPoints sprint
      }
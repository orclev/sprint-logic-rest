module Type.Generated 
  ( Team (..)
  , TeamGeneric (..)
  , Unique (..)
  , Sprint (..)
  , SprintGeneric (..)
  , EntityField (..)
  , migrateAll
  ) where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Type.Core

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Team
    ident ResourceIdent
    name Text
    UniqueName name
    UniqueTeamIdent ident
    deriving Show Eq Generic Typeable

Sprint
    team ResourceIdent
    ident ResourceIdent
    number Int
    people Int
    workDays Int
    vacationDays Int
    interruptHours Int Maybe
    plannedPoints Int
    deliveredPoints Int Maybe
    UniqueSprintIdent ident
    deriving Show Eq Generic Typeable
|]

instance JSONSchema Team where schema = gSchema
instance ToJSON Team
instance FromJSON Team

instance JSONSchema Sprint where schema = gSchema
instance ToJSON Sprint
instance FromJSON Sprint
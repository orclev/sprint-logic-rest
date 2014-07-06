module Type.Generated 
  ( Team (..)
  , TeamGeneric (..)
  , Unique (..)
  , Sprint (..)
  , SprintGeneric (..)
  , EntityField (..)
  , migrateAll
  , Checkmark (..)
  ) where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Type.Core
import Data.JSON.Schema (Schema (..))

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
    latest Checkmark nullable
    UniqueSprintIdent ident
    UniqueSprintNumber team number
    UniqueSprintLatest team latest !force
    deriving Show Eq Generic Typeable
|]

instance JSONSchema Checkmark where
  schema _ = Boolean

instance ToJSON Checkmark where
  toJSON Active = Bool True
  toJSON Inactive = Bool False

instance FromJSON Checkmark where
  parseJSON (Bool True) = pure Active
  parseJSON (Bool False) = pure Inactive
  parseJSON _ = mzero

instance JSONSchema Team where schema = gSchema
instance ToJSON Team
instance FromJSON Team

instance JSONSchema Sprint where schema = gSchema
instance ToJSON Sprint
instance FromJSON Sprint
module Type.Generated 
  ( Team (..)
  , TeamGeneric (..)
  , Unique (..)
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
    UniqueIdent ident
    deriving Show Eq Generic Typeable
|]

instance JSONSchema Team where schema = gSchema
instance ToJSON Team
instance FromJSON Team
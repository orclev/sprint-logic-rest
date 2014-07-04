module Model where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Model.Core
import Data.Text
import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics

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

data CreateTeam = CreateTeam { name :: Text }
  deriving (Eq, Generic, Show, Typeable)

instance JSONSchema CreateTeam where schema = gSchema
instance ToJSON CreateTeam
instance FromJSON CreateTeam

data TeamError = InvalidName | TeamExists | UnknownError
  deriving (Eq, Generic, Ord, Show, Typeable)

instance JSONSchema TeamError where schema = gSchema
instance ToJSON TeamError
instance FromJSON TeamError
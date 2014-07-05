module Type.Sprint 
  ( Sprint (..)
  , SprintGeneric (..)
  , SprintError (..)
  , Unique (..)
  ) where

import Type.Core
import Type.Generated (Sprint (..), SprintGeneric (..), Unique (..))

data SprintError = InvalidName | SprintExists | UnknownError
  deriving (Eq, Generic, Ord, Show, Typeable)

instance JSONSchema SprintError where schema = gSchema
instance ToJSON SprintError
instance FromJSON SprintError
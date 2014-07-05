module Type.Sprint 
  ( Sprint (..)
  , SprintGeneric (..)
  , SprintError (..)
  , EntityField (..)
  , Unique (..)
  ) where

import Type.Core
import Type.Generated (Sprint (..), SprintGeneric (..), Unique (..), EntityField (..))

data SprintError = InvalidName | SprintExists | UnknownError
  deriving (Eq, Generic, Ord, Show, Typeable)

instance JSONSchema SprintError where schema = gSchema
instance ToJSON SprintError
instance FromJSON SprintError
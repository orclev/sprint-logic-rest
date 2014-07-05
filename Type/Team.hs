module Type.Team 
  ( Team (..)
  , TeamGeneric (..)
  , TeamError (..)
  , EntityField (..)
  , Unique (..)
  ) where

import Type.Core
import Type.Generated (Team (..), TeamGeneric (..), Unique (..), EntityField (..))

data TeamError = InvalidName | TeamExists | UnknownError
  deriving (Eq, Generic, Ord, Show, Typeable)

instance JSONSchema TeamError where schema = gSchema
instance ToJSON TeamError
instance FromJSON TeamError
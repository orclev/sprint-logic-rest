module Type.CreateTeam 
  ( CreateTeam (..)
  )where

import Type.Core

data CreateTeam = CreateTeam { teamName :: Text }
  deriving (Eq, Generic, Show, Typeable)

instance JSONSchema CreateTeam where schema = gSchema
instance ToJSON CreateTeam
instance FromJSON CreateTeam
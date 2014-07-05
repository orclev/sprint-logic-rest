module Type.CreateTeam 
  ( CreateTeam (..)
  )where

import Type.Core
import qualified Type.Team as T (Team (..), TeamGeneric (..))

data CreateTeam = CreateTeam { teamName :: Text }
  deriving (Eq, Generic, Show, Typeable)

instance JSONSchema CreateTeam where schema = gSchema
instance ToJSON CreateTeam
instance FromJSON CreateTeam

instance MonadIO m => CreateAble m CreateTeam T.Team where
  createFrom (CreateTeam name) = do
    rid <- liftIO $ randomRid TeamR 
    return $ T.Team rid name
module Api.Version 
  ( resource
  , buildVersion
  ) where

import Type.Core
import Type.Version
import Data.Text (Text, pack)
import Rest 
import Type.Api
import qualified Rest.Resource as R

type VersionHandler = ReaderT () SiteApi
data Version = Version { buildVersion :: Text } deriving (Eq, Generic, Show, Typeable)
instance JSONSchema Version where schema = gSchema
instance ToJSON Version

genVersion

constVersion :: Version
constVersion = Version $ pack version

resource :: Resource SiteApi VersionHandler () Void Void
resource = mkResourceReader
  { R.name = "version"
  , R.schema = singleton () (unnamedSingle (\_ -> ()))
  , R.get = Just get
  }

get :: Handler VersionHandler
get = mkConstHandler (jsonO . someO) (return constVersion)

module Type.Core 
  ( ResourceKind (..)
  , ResourceIdent (..)
  , CreateAble (createFrom)
  , randomRid
  , MonadIO
  , liftIO
  , module E
  ) where

import Database.Persist.TH (derivePersistField)
import Data.Text as E (Text, pack, unpack, append)
import Data.UUID (UUID (..), toWords)
import Data.UUID.V4 (nextRandom)
import Data.List (genericIndex)
import Data.Bits (shiftL)
import Rest.Info (Info (describe))
import Rest.Types.ShowUrl (ShowUrl (showUrl))
import Control.Monad.Trans (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable as E (Typeable)
import GHC.Generics as E (Generic)
import Data.Aeson as E
import Data.JSON.Schema as E (JSONSchema (schema), gSchema)

class MonadIO m => CreateAble m a b where
  createFrom :: a -> m b

data ResourceKind = TeamR | SprintR deriving (Eq, Typeable, Generic)

instance Show ResourceKind where
  show TeamR = "team"
  show SprintR = "sprint"

instance Read ResourceKind where
  readsPrec _ "team" = [(TeamR, "")]
  readsPrec _ "sprint" = [(SprintR, "")]

derivePersistField "ResourceKind"

instance JSONSchema ResourceKind where schema = gSchema
instance ToJSON ResourceKind
instance FromJSON ResourceKind

data ResourceIdent = RID ResourceKind Text deriving (Eq, Typeable, Generic)
derivePersistField "ResourceIdent"

instance Show ResourceIdent where
  show (RID tag val) = show tag ++ "_" ++ unpack val

instance Read ResourceIdent where
  readsPrec _ mrid = let (tag, val) = break (=='_') mrid in [(RID (read tag) (pack . drop 1 . fst $ break (==' ') val), snd $ break (==' ') val)]

instance JSONSchema ResourceIdent where schema = gSchema
instance ToJSON ResourceIdent where
  toJSON = toJSON . show
instance FromJSON ResourceIdent where
  parseJSON (String val) = return . read $ unpack val

instance Info ResourceIdent where
  describe _ = "identifier"

instance ShowUrl ResourceIdent where
  showUrl = show

randomRid :: ResourceKind -> IO ResourceIdent
randomRid tag = nextRandom >>= return . encode
  where
    encode = RID tag . encodeUUID

encodeUUID :: UUID -> Text
encodeUUID = encodeBase62 . convert . toWords
  where
    convert (d,c,b,a) = (toInteger a) + (shiftL (toInteger b) 32) + (shiftL (toInteger c) 64) + (shiftL (toInteger d) 96)

charSet :: String
charSet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

encodeBase62 :: Integer -> Text
encodeBase62 = pack . encodeBase62'

encodeBase62' :: Integer -> String
encodeBase62' a | a <= 62 = [charSet `genericIndex` a]
                | otherwise = let (r, i) = a `divMod` 62 in charSet `genericIndex` i : encodeBase62' r

module Model.Core where

import Database.Persist.TH
import Data.Text (Text, pack, unpack, append)
import Data.UUID
import Data.UUID.V4
import Data.List (genericIndex)
import Data.Bits (shiftL)
import Data.Typeable
import GHC.Generics
import qualified Data.Aeson as A
import Data.JSON.Schema

data ResourceKind = TeamR | PostR deriving (Eq, Typeable, Generic)

instance Show ResourceKind where
  show TeamR = "team"
  show PostR = "post"

instance Read ResourceKind where
  readsPrec _ "team" = [(TeamR, "")]
  readsPrec _ "post" = [(PostR, "")]

derivePersistField "ResourceKind"

instance JSONSchema ResourceKind where schema = gSchema
instance A.ToJSON ResourceKind
instance A.FromJSON ResourceKind

data ResourceIdent = RID ResourceKind Text deriving (Eq, Typeable, Generic)
derivePersistField "ResourceIdent"

instance Show ResourceIdent where
  show (RID tag val) = show tag ++ "_" ++ unpack val

instance Read ResourceIdent where
  readsPrec _ mrid = let (tag, val) = break (=='_') mrid in [(RID (read tag) (pack . drop 1 . fst $ break (==' ') val), snd $ break (==' ') val)]

instance JSONSchema ResourceIdent where schema = gSchema
instance A.ToJSON ResourceIdent where
  toJSON = A.toJSON . show
instance A.FromJSON ResourceIdent where
  parseJSON (A.String val) = return . read $ unpack val

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

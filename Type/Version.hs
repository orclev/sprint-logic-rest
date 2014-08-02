module Type.Version where

import Data.List (reverse, drop, foldr)
import Control.Applicative ((<$>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Distribution.Verbosity (silent)
import Distribution.Version (Version (..), versionBranch)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (packageDescription, package)
import Distribution.PackageDescription.Parse

genVersion :: Q [Dec]
genVersion = do
  pkg <- runIO $ readPackageDescription silent "sprint-logic-rest.cabal"
  let version = formatVersion $ pkgVersion $ package $ packageDescription pkg
  (:[]) <$> valD (varP $ mkName "version") (normalB $ litE $ stringL version) []

formatVersion :: Version -> String
formatVersion = reverse . drop 1 . foldr (\xs x -> x ++ ('.':xs)) [] . map show . versionBranch
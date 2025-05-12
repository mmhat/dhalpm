module Types where

import Archlinux.Alpm (AlpmPkgName)
import Data.Vector (Vector)
import Relude

import Types.Dhall

data DhalpmException
    = ConflictingDatabaseDefinitions (NonEmpty Database)
    | NoProviderFound AlpmPkgName String (Vector AlpmPkgName) [AlpmPkgName]
    | PackageNotFound Package
    | InvalidVersionRange Text String
    deriving (Show)

instance Exception DhalpmException

module Types where

import Archlinux.Alpm (AlpmPkgName)
import Data.Vector (Vector)
import Effectful.Log (LogLevel)
import Relude

import Types.Dhall

data DhalpmException
    = ConflictingDatabaseDefinitions (NonEmpty Database)
    | NoProviderFound AlpmPkgName String (Vector Text) [Text]
    | PackageNotFound Package
    | InvalidVersionRange Text String
    deriving (Show)

instance Exception DhalpmException

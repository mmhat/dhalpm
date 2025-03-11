module Types where

import Effectful.Log (LogLevel)
import Relude

import Archlinux.Alpm (AlpmPkgName)
import Types.Dhall

-- | Command line arguments
data Options = Options
    { optionsVerbose :: Bool
    , optionsLogLevel :: LogLevel
    , optionsConfig :: (Maybe FilePath)
    }

data DhalpmException
    = ConflictingDatabaseDefinitions [Database]
    | NoProviderFound AlpmPkgName String [Text] [Text]
    | PackageNotFound Package
    | InvalidVersionRange Text String
    deriving (Show)

instance Exception DhalpmException

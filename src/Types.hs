module Types where

import RIO

import RIO.Process

import Archlinux.Alpm (AlpmPkgName)
import Types.Dhall



-- | Command line arguments
data Options = Options
    { optionsVerbose  :: Bool
    , optionsLogLevel :: LogLevel
    , optionsConfig   :: (Maybe FilePath)
    }

data App = App
    { appLogFunc        :: LogFunc
    , appProcessContext :: ProcessContext
    -- App-specific CLI options and configuration
    , appOptions        :: Options
    -- Other app-specific configuration
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\app lf -> app { appLogFunc = lf })

instance HasProcessContext App where
    processContextL = lens appProcessContext (\app pc -> app { appProcessContext = pc })

data DhalpmException
    = ConflictingDatabaseDefinitions [Database]
    | NoProviderFound AlpmPkgName String [Text] [Text]
    | PackageNotFound Package
    | InvalidVersionRange Text String
    deriving Show

instance Exception DhalpmException

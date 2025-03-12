{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Dhall where

import Data.Default.Class (Default)
import Data.Either.Validation (Validation (..))
import Data.Vector (Vector)
import Dhall (Decoder (..), Encoder (..), FromDhall (..), ToDhall (..))
import Dhall.Deriving (Codec (..), DropPrefix, Field, SpinalCase, type (<<<))
import Relude hiding (Any)

import Data.Default.Class qualified as Default
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Dhall qualified
import Dhall.Core qualified
import Dhall.Map qualified
import Dhall.Src qualified

import Archlinux.Alpm (
    AlpmPkgName,
    emptyAlpmPkgName,
    parseAlpmPkgName,
    unAlpmPkgName,
 )

default (Text)

data Config = Config
    { configRootDir :: FilePath
    , configDatabaseDir :: FilePath
    , configPackages :: Vector Package
    }
    deriving stock (Generic, Show)
    deriving
        (FromDhall, ToDhall)
        via Codec (Field (SpinalCase <<< DropPrefix "config")) Config

data Package = Package
    { packageName :: AlpmPkgName
    , packageVersions :: Versions
    , packageSigcheck :: SiglevelCheck
    , packageSigtrust :: SiglevelTrust
    , packageDatabases :: Vector Database
    , packageProviders :: Vector Text
    , packageBuild :: Maybe Build
    }
    deriving stock (Generic, Show)
    deriving
        (FromDhall, ToDhall)
        via Codec (Field (SpinalCase <<< DropPrefix "package")) Package

instance Default Package where
    def =
        Package
            { packageName = emptyAlpmPkgName
            , packageVersions = Default.def
            , packageSigcheck = Default.def
            , packageSigtrust = Default.def
            , packageDatabases = Vector.empty
            , packageProviders = Vector.empty
            , packageBuild = Nothing
            }

packageNameToString :: Package -> String
packageNameToString = unAlpmPkgName . packageName

packageNameToText :: Package -> Text
packageNameToText = Text.pack . packageNameToString

data Build = Build
    { buildPath :: FilePath
    , -- , buildVersion :: Version
      -- , buildDependencies :: Vector Source
      buildScript :: Text
    }
    deriving stock (Generic, Show)
    deriving
        (FromDhall, ToDhall)
        via Codec (Field (SpinalCase <<< DropPrefix "build")) Build

data Database = Database
    { databaseName :: Text
    , databaseSigcheck :: SiglevelCheck
    , databaseSigtrust :: SiglevelTrust
    , databaseServers :: Vector Text -- TODO: NonEmpty/NonNull; URI filetype (modern-uri ?)
    -- , databaseDependencies :: Vector Database
    }
    deriving stock (Eq, Generic, Show)
    deriving
        (FromDhall, ToDhall)
        via Codec (Field (SpinalCase <<< DropPrefix "database")) Database

instance Default Database where
    def =
        Database
            { databaseName = mempty
            , databaseSigcheck = Default.def
            , databaseSigtrust = Default.def
            , databaseServers = Vector.empty
            }

databaseNameToString :: Database -> String
databaseNameToString = Text.unpack . databaseNameToText

databaseNameToText :: Database -> Text
databaseNameToText = databaseNameToText

data SiglevelCheck
    = CheckNever
    | CheckOptional
    | CheckRequired
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

instance Default SiglevelCheck where
    def = CheckOptional

data SiglevelTrust
    = TrustAll
    | TrustMarginal
    | TrustFull
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

instance Default.Default SiglevelTrust where
    def = TrustFull

data Versions
    = Lt Version
    | Le Version
    | Eq Version
    | Ge Version
    | Gt Version
    | Any
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

instance Default Versions where
    def = Any

data Version = Version
    { versionEpoch :: Maybe Natural
    , versionVersion :: Text
    , versionRel :: Natural
    , versionSubrel :: Maybe Natural
    }
    deriving stock (Generic, Show)
    deriving
        (FromDhall, ToDhall)
        via Codec (Field (SpinalCase <<< DropPrefix "version")) Version

instance Default Version where
    def =
        Version
            { versionEpoch = Nothing
            , versionVersion = Text.empty
            , versionRel = 1
            , versionSubrel = Nothing
            }

instance FromDhall AlpmPkgName where
    autoWith normalizer = Decoder{expected, extract}
        where
            expected = Dhall.expected decoder

            extract expr = case Dhall.extract decoder expr of
                Failure es -> Failure es
                Success xs -> case parseAlpmPkgName xs of
                    Left e ->
                        Dhall.extractError
                            . Text.pack
                            . displayException
                            $ e
                    Right n -> Success n

            decoder :: Decoder Text
            decoder = autoWith @Text normalizer

instance ToDhall AlpmPkgName where
    injectWith normalizer = unAlpmPkgName >$< injectWith normalizer

embedDefault
    :: (Default a) => Encoder a -> Dhall.Core.Expr Dhall.Src.Src Void
embedDefault enc =
    Dhall.Core.RecordLit
        $ Dhall.Map.fromList
            [ ("Type", Dhall.Core.makeRecordField $ Dhall.declared enc)
            , ("default", Dhall.Core.makeRecordField $ Dhall.embed enc Default.def)
            ]

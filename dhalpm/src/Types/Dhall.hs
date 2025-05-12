{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Dhall (
    readConfig,
    parseConfig,
    inputConfig,
    getSubstitutions,
    substitutions,
    Config (..),
    Database (..),
    Package (..),
    Build (..),
    Version (..),
    Versions (..),
    SiglevelCheck (..),
    SiglevelTrust (..),
) where

import Archlinux.Alpm (
    AlpmPkgName,
    emptyAlpmPkgName,
    unAlpmPkgName,
 )
import Archlinux.Alpm.Package.Types (parsePackageNameFromText)
import Data.Default.Class (Default)
import Data.Either.Validation (Validation (..))
import Data.Text.Short (ShortText)
import Data.Vector (Vector)
import Dhall (
    Decoder (..),
    Encoder (..),
    Expector,
    FromDhall (..),
    ToDhall (..),
 )
import Dhall.Core (Expr, Import)
import Dhall.Deriving (Codec (..), DropPrefix, Field, SpinalCase, type (<<<))
import Dhall.Src (Src)
import Dhall.Substitution (Substitutions)
import Effectful
import Effectful.Exception (throwIO)
import Relude hiding (Any)
import Relude.Extra.Lens (set)

import Data.Default.Class qualified as Default
import Data.Text qualified as Text
import Data.Text.Short qualified as ShortText
import Data.Vector qualified as Vector
import Dhall qualified
import Dhall.Core qualified
import Dhall.Map qualified

default (Text)

readConfig :: (IOE :> es) => FilePath -> Eff es Config
readConfig fp = do
    substitutions' <- getSubstitutions
    let
        settings =
            set Dhall.substitutions substitutions'
                $ Dhall.defaultEvaluateSettings
    liftIO (Dhall.inputFileWithSettings settings (Dhall.auto @Config) fp)

parseConfig :: (IOE :> es) => Text -> Eff es Config
parseConfig text = do
    substitutions' <- getSubstitutions
    let
        settings =
            set Dhall.substitutions substitutions'
                $ Dhall.defaultInputSettings
    liftIO (Dhall.inputWithSettings settings (Dhall.auto @Config) text)

inputConfig :: (IOE :> es) => Expr Src Import -> Eff es Config
inputConfig expression = do
    substitutions' <- getSubstitutions
    let
        decoder :: Decoder Config
        decoder = Dhall.auto

        settings =
            set Dhall.substitutions substitutions'
                $ Dhall.defaultInputSettings

    liftIO (Dhall.fromExprWithSettings settings decoder expression)

getSubstitutions :: Eff es (Substitutions Src Void)
getSubstitutions = case substitutions of
    Failure e -> throwIO e
    Success substitutions' -> pure substitutions'

substitutions :: Expector (Substitutions Src Void)
substitutions =
    Dhall.Map.fromList
        <$> sequenceA
            [ ("Build/Type",) <$> Dhall.expected (Dhall.auto @Build)
            , ("Config/Type",) <$> Dhall.expected (Dhall.auto @Config)
            , ("Database/Type",) <$> Dhall.expected (Dhall.auto @Database)
            , ("Package/Type",) <$> Dhall.expected (Dhall.auto @Package)
            , ("SiglevelCheck/Type",) <$> Dhall.expected (Dhall.auto @SiglevelCheck)
            , ("SiglevelTrust/Type",) <$> Dhall.expected (Dhall.auto @SiglevelTrust)
            , ("Versions/Type",) <$> Dhall.expected (Dhall.auto @Versions)
            , ("Version/Type",) <$> Dhall.expected (Dhall.auto @Version)
            , --
              pure ("Database", embedDefault (Dhall.inject @Database))
            , pure ("Package", embedDefault (Dhall.inject @Package))
            , pure ("SiglevelCheck", embedDefault (Dhall.inject @SiglevelCheck))
            , pure ("SiglevelTrust", embedDefault (Dhall.inject @SiglevelTrust))
            , pure ("Versions", embedDefault (Dhall.inject @Versions))
            -- , pure ("Version"      , embedDefault (Dhall.inject @Version      ))
            ]

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
    , packageProviders :: Vector AlpmPkgName
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

packageNameToShortText :: Package -> ShortText
packageNameToShortText = unAlpmPkgName . packageName

packageNameToText :: Package -> Text
packageNameToText = ShortText.toText . packageNameToShortText

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
    , versionVersion :: ShortText
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
            , versionVersion = ShortText.empty
            , versionRel = 1
            , versionSubrel = Nothing
            }

instance FromDhall AlpmPkgName where
    autoWith normalizer = Decoder{expected, extract}
        where
            expected = Dhall.expected decoder

            extract expr = case Dhall.extract decoder expr of
                Failure es -> Failure es
                Success xs -> case parsePackageNameFromText xs of
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

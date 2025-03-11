{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Dhall where

import Data.Either.Validation (Validation (..))
import Dhall hiding (normalizer)
import RIO

import Data.Default.Class qualified as DC
import Dhall.Core qualified
import Dhall.Map qualified
import Dhall.Src qualified
import RIO.Text qualified as T

import Archlinux.Alpm (
    AlpmPkgName,
    emptyAlpmPkgName,
    parseAlpmPkgName,
    unAlpmPkgName,
 )

default (Text)

data Config = Config
    { rootDir :: FilePath
    , databaseDir :: FilePath
    , packages :: Vector Package
    }
    deriving (Generic, Show)

instance FromDhall Config
instance ToDhall Config

data Package = Package
    { name :: AlpmPkgName
    , versions :: Versions
    , sigcheck :: SiglevelCheck
    , sigtrust :: SiglevelTrust
    , databases :: Vector Database
    , providers :: [Text]
    , build :: Maybe Build
    }
    deriving (Generic, Show)

instance FromDhall Package
instance ToDhall Package

instance DC.Default Package where
    def =
        Package
            { name = emptyAlpmPkgName
            , versions = DC.def
            , sigcheck = DC.def
            , sigtrust = DC.def
            , databases = mempty
            , providers = mempty
            , build = Nothing
            }

packageNameToString :: Package -> String
packageNameToString Package{name} = unAlpmPkgName name

packageNameToText :: Package -> Text
packageNameToText = T.pack . packageNameToString

data Build = Build
    { path :: FilePath
    , -- , version :: Version
      -- , dependencies :: Vector Source
      script :: Text
    }
    deriving (Generic, Show)

instance FromDhall Build
instance ToDhall Build

data Database = Database
    { name :: Text
    , sigcheck :: SiglevelCheck
    , sigtrust :: SiglevelTrust
    , servers :: Vector Text -- TODO: NonEmpty/NonNull; URI filetype (modern-uri ?)
    -- , dependencies :: Vector Database
    }
    deriving (Eq, Generic, Show)

instance FromDhall Database
instance ToDhall Database

instance DC.Default Database where
    def =
        Database
            { name = mempty
            , sigcheck = DC.def
            , sigtrust = DC.def
            , servers = mempty
            }

databaseNameToString :: Database -> String
databaseNameToString = T.unpack . databaseNameToText

databaseNameToText :: Database -> Text
databaseNameToText Database{name} = name

data SiglevelCheck
    = CheckNever
    | CheckOptional
    | CheckRequired
    deriving (Eq, Generic, Show)

instance FromDhall SiglevelCheck
instance ToDhall SiglevelCheck

instance DC.Default SiglevelCheck where
    def = CheckOptional

data SiglevelTrust
    = TrustAll
    | TrustMarginal
    | TrustFull
    deriving (Eq, Generic, Show)

instance FromDhall SiglevelTrust
instance ToDhall SiglevelTrust

instance DC.Default SiglevelTrust where
    def = TrustFull

data Versions
    = Lt Version
    | Le Version
    | Eq Version
    | Ge Version
    | Gt Version
    | Any
    deriving (Generic, Show)

instance FromDhall Versions
instance ToDhall Versions

instance DC.Default Versions where
    def = Any

data Version = Version
    { epoch :: Maybe Natural
    , version :: Text
    , rel :: Natural
    , subrel :: Maybe Natural
    }
    deriving (Generic, Show)

instance FromDhall Version
instance ToDhall Version

instance DC.Default Version where
    def =
        Version
            { epoch = Nothing
            , version = mempty
            , rel = 1
            , subrel = Nothing
            }

instance FromDhall AlpmPkgName where
    autoWith normalizer = Decoder{..}
        where
            expected = Dhall.expected (autoWith @Text normalizer)
            extract expr = case Dhall.extract (autoWith @Text normalizer) expr of
                Failure es -> Failure es
                Success xs -> case parseAlpmPkgName xs of
                    Left e -> extractError $ T.pack $ displayException e
                    Right n -> Success n

instance ToDhall AlpmPkgName where
    injectWith normalizer = unAlpmPkgName >$< injectWith normalizer

embedDefault
    :: (DC.Default a) => Encoder a -> Dhall.Core.Expr Dhall.Src.Src Void
embedDefault enc =
    Dhall.Core.RecordLit
        $ Dhall.Map.fromList
            [ ("Type", Dhall.Core.makeRecordField $ Dhall.declared enc)
            , ("default", Dhall.Core.makeRecordField $ Dhall.embed enc DC.def)
            ]

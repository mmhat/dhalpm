{-# LANGUAGE TemplateHaskell #-}

module Options (
    Command,
    Options (..),
    parse,
    optionsParser,
) where

import Log (LogLevel (..))
import Options.Applicative.Simple
import Prelude

import PackageInfo_dhalpm (version)

type Command = ()

data Options = Options
    { optionsLogLevel :: LogLevel
    , optionsConfig :: FilePath
    }

parse :: IO (Options, Command)
parse =
    simpleOptions
        $(simpleVersion version)
        ""
        "A declarative package manager based on libalpm"
        optionsParser
        empty

optionsParser :: Parser Options
optionsParser =
    Options
        <$> (option logLevelReader)
            ( short 'l'
                <> long "log-level"
                <> metavar "LEVEL"
                <> value LogInfo
                <> completeWith
                    [ "trace"
                    , "info"
                    , "attention"
                    ]
            )
        <*> strOption
            ( short 'c'
                <> long "config-file"
                <> metavar "FILE"
                <> value "config.dhall"
                <> help "Use this configuration file"
                <> action "file"
            )

logLevelReader :: ReadM LogLevel
logLevelReader = maybeReader $ \case
    "trace" -> Just LogTrace
    "info" -> Just LogInfo
    "attention" -> Just LogAttention
    _ -> Nothing

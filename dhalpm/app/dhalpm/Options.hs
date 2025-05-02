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
    , optionsConfig :: (Maybe FilePath)
    }

parse :: IO (Options, Command)
parse =
    simpleOptions
        $(simpleVersion version)
        ""
        "Launch and query autostart profiles"
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
        <*> (optional . strOption)
            ( short 'c'
                <> long "config-file"
                <> metavar "FILE"
                <> help "Use this configuration file"
                <> action "file"
            )

logLevelReader :: ReadM LogLevel
logLevelReader = maybeReader $ \case
    "trace" -> Just LogTrace
    "info" -> Just LogInfo
    "attention" -> Just LogAttention
    _ -> Nothing

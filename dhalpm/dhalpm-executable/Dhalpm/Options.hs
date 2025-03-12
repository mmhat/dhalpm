{-# LANGUAGE TemplateHaskell #-}

module Library.Options (
    Options (..),
    Command (..),
    parse,
) where

import Library.Import

import PackageInfo_dhalpm qualified

import Options.Applicative.Simple

data Options = Options

type Command = ()

parse :: IO (Options, Command)
parse =
    simpleOptions
        $(simpleVersion PackageInfo_dhalpm.version)
        ""
        PackageInfo_dhalpm.synopsis
        optionsParser
        $ do
            empty

optionsParser :: Parser Options
optionsParser = pure Options

module Main (main) where

import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog)
import Effectful.Process.Typed (runTypedProcess)
import Log.Backend.StandardOutput (withStdOutLogger)
import Prelude

import Run

import Options

main :: IO ()
main = do
    (options, _command) <- Options.parse
    let
        configFile = optionsConfig options
        logLevel = optionsLogLevel options
    withStdOutLogger $ \logger -> do
        runEff
            . runLog "" logger logLevel
            . runFileSystem
            . runTypedProcess
            $ run configFile

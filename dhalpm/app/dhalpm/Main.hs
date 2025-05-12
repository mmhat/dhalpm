module Main (main) where

import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Temporary (runTemporary)
import Log.Backend.StandardOutput (withStdOutLogger)
import Prelude

import Options
import Run

main :: IO ()
main = do
    (options, _command) <- Options.parse
    let
        configFile = optionsConfig options
        logLevel = optionsLogLevel options
    withStdOutLogger $ \logger -> do
        runEff
            . runFileSystem
            . runLog "" logger logLevel
            . runTemporary
            . runTypedProcess
            $ runFromFile configFile

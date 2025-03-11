module Main (main) where

import Effectful
import Effectful.Log
import Effectful.Process.Typed
import Relude

import Run

main :: IO ()
main = runEff $ run
